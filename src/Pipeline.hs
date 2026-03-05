{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Pipeline (module Pipeline) where

import Algebra.Graph.Undirected qualified as Undirected
import Core
import Data.Containers.ListUtils (nubOrd)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet (difference, union)
import Data.HashSet qualified as HashSet
import Data.List ((!?))
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set qualified as Set
import Effectful.Error.Static
import Effectful.State.Static.Local
import Pre
import Stage.CVar qualified as CVar
import Stage.LVar qualified as LVar
import Stage.LVarMon qualified as LVarMon
import Stage.X86Int qualified as X86Int
import Stage.X86Var qualified as X86Var

type PipelineErr :: Type
data PipelineErr
  = UnboundRenaming
  deriving stock (Eq, Show)

-- | \(O(n)\) Alpha-rename to ensure uniqueness of variables.
passUniquify :: (Error PipelineErr :> es, Gensym :> es) => LVar.Expr -> Eff es LVar.Expr
passUniquify = loop mempty
 where
  loop env = \case
    e@(LVar.Lit _) -> pure e
    LVar.Var name -> case HashMap.lookup name env of
      Just name' -> pure (LVar.Var name')
      Nothing -> throwError UnboundRenaming
    LVar.Let name expr body -> do
      name' <- MkName <$> gensym (getName name <> ".")
      expr' <- loop env expr
      body' <- loop (HashMap.insert name name' env) body
      pure $ LVar.Let name' expr' body'
    e@(LVar.NulApp _) -> pure e
    LVar.UnApp op a -> LVar.UnApp op <$> loop env a
    LVar.BinApp op a b -> LVar.BinApp op <$> loop env a <*> loop env b

-- | \(O(n)\) Transform the language into Monadic IR.
passRemoveComplexOperands :: (Gensym :> es) => LVar.Expr -> Eff es LVarMon.Expr
passRemoveComplexOperands = \case
  LVar.Lit n -> pure $ LVarMon.Atom (Lit n)
  LVar.Var n -> pure $ LVarMon.Atom (Var n)
  LVar.Let name expr body -> do
    expr' <- passRemoveComplexOperands expr
    body' <- passRemoveComplexOperands body
    pure $ LVarMon.Let name expr' body'
  LVar.NulApp op -> pure $ LVarMon.NulApp op
  LVar.UnApp op a -> rcoUnOp op a
  LVar.BinApp op a b -> rcoBinOp op a b
 where
  genTmpName = MkName <$> gensym "tmp."

  rcoAtom = \case
    LVar.Lit n -> Just (Lit n)
    LVar.Var n -> Just (Var n)
    _ -> Nothing

  rcoUnOp op expr = case rcoAtom expr of
    Just atom -> pure $ LVarMon.UnApp op atom
    Nothing -> do
      expr' <- passRemoveComplexOperands expr
      tmp <- genTmpName
      pure $ LVarMon.Let tmp expr' (LVarMon.UnApp op (Var tmp))

  rcoBinOp op expra exprb = case (rcoAtom expra, rcoAtom exprb) of
    (Just ra, Just rb) -> pure $ LVarMon.BinApp op ra rb
    (Nothing, Just rb) -> do
      ra <- passRemoveComplexOperands expra
      tmpa <- genTmpName
      pure $ LVarMon.Let tmpa ra (LVarMon.BinApp op (Var tmpa) rb)
    (Just ra, Nothing) -> do
      rb <- passRemoveComplexOperands exprb
      tmpb <- genTmpName
      pure $ LVarMon.Let tmpb rb (LVarMon.BinApp op ra (Var tmpb))
    (Nothing, Nothing) -> do
      ra <- passRemoveComplexOperands expra
      rb <- passRemoveComplexOperands exprb
      tmpa <- genTmpName
      tmpb <- genTmpName
      pure $ LVarMon.Let tmpa ra (LVarMon.Let tmpb rb (LVarMon.BinApp op (Var tmpa) (Var tmpb)))

-- | \(O(n)\) Transform expression-based IR into statement-based.
passExplicateControl :: LVarMon.Expr -> CVar.Tail
passExplicateControl = \case
  LVarMon.Atom atm -> CVar.Return (CVar.Atom atm)
  LVarMon.Let name expr body -> assign name expr (passExplicateControl body)
  LVarMon.NulApp op -> CVar.Return (CVar.NulApp op)
  LVarMon.UnApp op a -> CVar.Return (CVar.UnApp op a)
  LVarMon.BinApp op a b -> CVar.Return (CVar.BinApp op a b)
 where
  assign name expr cont = case expr of
    LVarMon.Atom atm -> CVar.Seq (CVar.Assign name (CVar.Atom atm)) cont
    LVarMon.Let name' expr' body -> assign name' expr' (assign name body cont)
    LVarMon.NulApp op -> CVar.Seq (CVar.Assign name (CVar.NulApp op)) cont
    LVarMon.UnApp op a -> CVar.Seq (CVar.Assign name (CVar.UnApp op a)) cont
    LVarMon.BinApp op a b -> CVar.Seq (CVar.Assign name (CVar.BinApp op a b)) cont

-- | \(O(n)\) Lower IR into x86 instructions with variables.
passSelectInstructions :: CVar.Tail -> X86Var.Block
passSelectInstructions = \case
  CVar.Return (CVar.Atom atom) -> X86Var.MkBlock [MovQ (fromAtom atom) (X86Var.Reg RAX)]
  CVar.Return (CVar.NulApp op) -> X86Var.MkBlock $ fromNulOp op
  CVar.Return (CVar.UnApp op a) -> X86Var.MkBlock $ fromUnOp op a
  CVar.Return (CVar.BinApp op a b) -> X86Var.MkBlock $ fromBinOp op a b
  CVar.Seq stmt tail_ -> X86Var.MkBlock (fromStmt stmt ++ X86Var.getBlock (passSelectInstructions tail_))
 where
  fromAtom = \case
    Lit a -> X86Var.Imm a
    Var a -> X86Var.Var a

  fromNulOp Read = [CallQ "read_int" 0]

  fromUnOp Neg a =
    [ MovQ (fromAtom a) (X86Var.Reg RAX)
    , NegQ (X86Var.Reg RAX)
    ]

  fromBinOp op a b = case op of
    Add ->
      [ MovQ (fromAtom a) (X86Var.Reg RAX)
      , AddQ (fromAtom b) (X86Var.Reg RAX)
      ]
    Sub ->
      [ MovQ (fromAtom a) (X86Var.Reg RAX)
      , SubQ (fromAtom b) (X86Var.Reg RAX)
      ]

  rax2Var name = [MovQ (X86Var.Reg RAX) (X86Var.Var name)]

  fromStmt (CVar.Assign name expr) = case expr of
    CVar.Atom a -> [MovQ (fromAtom a) (X86Var.Var name)]
    CVar.NulApp op -> fromNulOp op ++ rax2Var name
    CVar.UnApp op a -> fromUnOp op a ++ rax2Var name
    CVar.BinApp op (Var name') rhs | name' == name -> case op of
      Add -> [AddQ (fromAtom rhs) (X86Var.Var name)]
      Sub -> [SubQ (fromAtom rhs) (X86Var.Var name)]
    CVar.BinApp op lhs (Var name') | name' == name -> case op of
      Add -> [AddQ (fromAtom lhs) (X86Var.Var name)]
      Sub -> [SubQ (fromAtom lhs) (X86Var.Var name)]
    CVar.BinApp op a b -> case op of
      Add ->
        [ MovQ (fromAtom a) (X86Var.Var name)
        , AddQ (fromAtom b) (X86Var.Var name)
        ]
      Sub ->
        [ MovQ (fromAtom a) (X86Var.Var name)
        , SubQ (fromAtom b) (X86Var.Var name)
        ]

type Liveness :: Type
data Liveness = MkLiveness
  { before :: HashSet X86Var.Arg
  , instr :: X86Var.Instr
  , after :: HashSet X86Var.Arg
  }
  deriving stock (Eq, Show)

instance Pretty Liveness where
  pretty l = pretty l.before <+> "→" <+> pretty l.instr <+> "→" <+> pretty l.after

type LivenessTrace :: Type
newtype LivenessTrace = MkLivenessTrace (List Liveness)
  deriving stock (Show)
  deriving newtype (Eq)

instance Pretty LivenessTrace where
  pretty (MkLivenessTrace trace) = case trace of
    [] -> "∅"
    x : xs -> pretty x.before <+> foldMap (\l -> "→" <+> pretty l.instr <+> "→" <+> pretty l.after <> space) (x : xs)

{- | \(O(n \log m)\) Block-level local liveness analysis.

TODO: Handle 'Jmp' when compiler starts generating multiple blocks.
-}
uncoverLive :: HashSet X86Var.Arg -> X86Var.Block -> LivenessTrace
uncoverLive prev (X86Var.MkBlock block) = MkLivenessTrace . snd $ mapAccumR liveness prev block
 where
  liveness after instr =
    let before = (after `difference` def instr) `union` use instr
     in (before, MkLiveness{before, instr, after})

  def = \case
    MovQ _ tgt -> argSet tgt
    AddQ _ tgt -> argSet tgt
    SubQ _ tgt -> argSet tgt
    NegQ tgt -> argSet tgt
    CallQ _ _ -> HashSet.fromList [X86Var.Reg reg | reg <- callerSaved]
    _ -> HashSet.empty

  use = \case
    MovQ src _ -> argSet src
    AddQ src tgt -> argSet src <> argSet tgt
    SubQ src tgt -> argSet src <> argSet tgt
    NegQ tgt -> argSet tgt
    CallQ _ n -> HashSet.fromList [X86Var.Reg reg | reg <- take n argumentPassing]
    _ -> HashSet.empty

  argSet = \case
    arg@(X86Var.Reg _) -> HashSet.singleton arg
    arg@(X86Var.Var _) -> HashSet.singleton arg
    arg@(X86Var.Deref _ _) -> HashSet.singleton arg
    X86Var.Imm _ -> HashSet.empty

type InterferenceGraph :: Type
type InterferenceGraph = Undirected.Graph X86Var.Arg

buildInterference :: LivenessTrace -> InterferenceGraph
buildInterference (MkLivenessTrace trace) = Undirected.edges (concatMap getEdges trace)
 where
  getEdges MkLiveness{instr, after} = case instr of
    MovQ s d ->
      [ (d, v)
      | v <- HashSet.toList after
      , v /= d
      , v /= s
      , d `canInterfere` v
      ]
    CallQ _ _ ->
      [ (X86Var.Reg reg, v)
      | reg <- callerSaved
      , v <- HashSet.toList after
      , v /= X86Var.Reg reg
      , X86Var.Reg reg `canInterfere` v
      ]
    _ ->
      [ (d, v)
      | d <- HashSet.toList (writes instr)
      , v <- HashSet.toList after
      , v /= d
      , d `canInterfere` v
      ]

  writes :: X86Var.Instr -> HashSet X86Var.Arg
  writes = \case
    AddQ _ tgt -> HashSet.singleton tgt
    SubQ _ tgt -> HashSet.singleton tgt
    NegQ tgt -> HashSet.singleton tgt
    MovQ _ tgt -> HashSet.singleton tgt
    PushQ _ -> HashSet.singleton (X86Var.Reg RSP)
    PopQ _ -> HashSet.singleton (X86Var.Reg RSP)
    _ -> mempty

  canInterfere = \cases
    (X86Var.Reg _) (X86Var.Reg _) -> True
    (X86Var.Var _) (X86Var.Var _) -> True
    (X86Var.Var _) (X86Var.Reg _) -> True
    (X86Var.Reg _) (X86Var.Var _) -> True
    _ _ -> False

-- | \(O(n^2 \log n)\) Color a graph by picking the least saturated node
colorGraph :: forall a. (Hashable a, Ord a) => [a] -> Undirected.Graph a -> HashMap.HashMap a Int
colorGraph vertices g =
  HashMap.fromList
    $ go
      (Set.fromList $ (0,) <$> vertices)
      (HashMap.fromList $ (,mempty) <$> vertices)
 where
  adj = HashMap.fromList $ Undirected.adjacencyList g
  -- nco - nc order. fst stores the ordering key (number of neighbors), snd stores the value
  go
    :: Set.Set (Int, a)
    -> HashMap.HashMap a (HashSet.HashSet Int)
    -> [(a, Int)]
  go (Set.maxView -> Nothing) _ = []
  go (Set.maxView -> Just ((_, v), nco)) nc =
    let ns = fromMaybe HashSet.empty $ nc HashMap.!? v
        neighbors = fromMaybe [] $ adj HashMap.!? v
        color = findColor ns 0
        nc' =
          HashMap.delete v
            $ flip HashMap.union nc
              . HashMap.fromList
            $ flip mapMaybe neighbors \to ->
              (to,) . HashSet.insert color <$> (nc HashMap.!? to)
        getNco ncc =
          Set.fromList
            $ flip mapMaybe neighbors \to ->
              (,to) . HashSet.size <$> (ncc HashMap.!? to)
        nco' = (nco `Set.difference` getNco nc) `Set.union` getNco nc'
     in (v, color) : go nco' nc'
  -- aka MEX
  findColor set i
    | HashSet.member i set = findColor set (i + 1)
    | otherwise = i

uncoverLocals :: X86Var.Block -> [Name]
uncoverLocals (X86Var.MkBlock xs) = nubOrd $ concatMap vars xs
 where
  vars = \case
    AddQ src tgt -> var src ++ var tgt
    SubQ src tgt -> var src ++ var tgt
    NegQ src -> var src
    MovQ src tgt -> var src ++ var tgt
    PushQ tgt -> var tgt
    PopQ tgt -> var tgt
    CallQ _ _ -> []
    Jmp _ -> []
    Syscall -> []
    RetQ -> []
  var (X86Var.Var name) = [name]
  var _ = []

-- | \(O(n^2 \log n)\) Allocate the registers based on @colorGraph@
passAllocateRegisters :: (State X86Int.Frame :> es) => InterferenceGraph -> X86Var.Block -> Eff es X86Int.Block
passAllocateRegisters ig block@(X86Var.MkBlock xs) =
  X86Int.MkBlock <$> for xs \case
    AddQ src tgt -> AddQ <$> alloc src <*> alloc tgt
    SubQ src tgt -> SubQ <$> alloc src <*> alloc tgt
    NegQ src -> NegQ <$> alloc src
    MovQ src tgt -> MovQ <$> alloc src <*> alloc tgt
    PushQ tgt -> PushQ <$> alloc tgt
    PopQ tgt -> PopQ <$> alloc tgt
    CallQ lbl n -> pure $ CallQ lbl n
    Jmp lbl -> pure $ Jmp lbl
    Syscall -> pure Syscall
    RetQ -> pure RetQ
 where
  regs = [RCX, RDX, RBX]
  colors = colorGraph (X86Var.Var <$> uncoverLocals block) ig
  alloc = \case
    X86Var.Imm n -> pure $ X86Int.Imm n
    X86Var.Reg reg -> pure $ X86Int.Reg reg
    X86Var.Deref n reg -> pure $ X86Int.Deref n reg
    v@(X86Var.Var name) ->
      case HashMap.lookup v colors of
        Just i | Just reg <- regs !? i -> pure $ X86Int.Reg reg
        _ -> spill name
  spill name = do
    X86Int.MkFrame{env, offset} <- get
    case HashMap.lookup name env of
      Just arg -> pure arg
      Nothing -> do
        let offset' = offset - 8
            arg = X86Int.Deref offset' RBP
        modify \s -> s{X86Int.env = HashMap.insert name arg env, X86Int.offset = offset'}
        pure arg

-- | \(O(n)\) *Obsolete* Replace variables with stack locations relative to base pointer.
passAssignHomes :: (State X86Int.Frame :> es) => X86Var.Block -> Eff es X86Int.Block
passAssignHomes (X86Var.MkBlock xs) =
  X86Int.MkBlock <$> for xs \case
    AddQ src tgt -> AddQ <$> spill src <*> spill tgt
    SubQ src tgt -> SubQ <$> spill src <*> spill tgt
    NegQ tgt -> NegQ <$> spill tgt
    MovQ src tgt -> MovQ <$> spill src <*> spill tgt
    PushQ tgt -> PushQ <$> spill tgt
    PopQ tgt -> PopQ <$> spill tgt
    CallQ lbl n -> pure $ CallQ lbl n
    Jmp lbl -> pure $ Jmp lbl
    Syscall -> pure Syscall
    RetQ -> pure RetQ
 where
  spill = \case
    X86Var.Imm n -> pure $ X86Int.Imm n
    X86Var.Reg reg -> pure $ X86Int.Reg reg
    X86Var.Deref n reg -> pure $ X86Int.Deref n reg
    X86Var.Var name -> do
      X86Int.MkFrame{env, offset} <- get
      case HashMap.lookup name env of
        Just arg -> pure arg
        Nothing -> do
          let offset' = offset - 8
              arg = X86Int.Deref offset' RBP
          modify \s -> s{X86Int.env = HashMap.insert name arg env, X86Int.offset = offset'}
          pure arg

{- | \(O(n)\) Patch instructions to make sure that each instruction adheres to the restriction that at most one argument
an instruction may be a memory reference.
-}
passPatchInstructions :: X86Int.Block -> X86Int.Block
passPatchInstructions (X86Int.MkBlock xs) = X86Int.MkBlock (concatMap patch xs)
 where
  patch = \case
    AddQ lhs@(X86Int.Deref _ _) rhs@(X86Int.Deref _ _) ->
      [ MovQ lhs (X86Int.Reg RAX)
      , AddQ (X86Int.Reg RAX) rhs
      ]
    SubQ lhs@(X86Int.Deref _ _) rhs@(X86Int.Deref _ _) ->
      [ MovQ lhs (X86Int.Reg RAX)
      , SubQ (X86Int.Reg RAX) rhs
      ]
    MovQ lhs@(X86Int.Deref _ _) rhs@(X86Int.Deref _ _) ->
      [ MovQ lhs (X86Int.Reg RAX)
      , MovQ (X86Int.Reg RAX) rhs
      ]
    i -> [i]

-- | \(O(1)\) Generate prelude and conclusion and connect the blocks with jumps.
passGeneratePreludeAndConclusion :: Platform -> Int -> X86Int.Block -> X86Int.Program
passGeneratePreludeAndConclusion p frameSize (X86Int.MkBlock main) =
  let lblMain_ = p `resolveLabel` lblMain
      lblPrelude_ = p `resolveLabel` lblPrelude
      lblConclusion_ = p `resolveLabel` lblConclusion
      prelude =
        [ PushQ (X86Int.Reg RBP)
        , MovQ (X86Int.Reg RSP) (X86Int.Reg RBP)
        ]
          ++ ([SubQ (X86Int.Imm frameSize) (X86Int.Reg RSP) | frameSize /= 0]) -- allocate
      conclusion =
        ([AddQ (X86Int.Imm frameSize) (X86Int.Reg RSP) | frameSize /= 0]) -- deallocate
          ++ [PopQ (X86Int.Reg RBP)]
      exit =
        [ MovQ (X86Int.Imm (exitSyscall p)) (X86Int.Reg RAX)
        , MovQ (X86Int.Imm 0) (X86Int.Reg RDI)
        , Syscall
        ]
   in X86Int.MkProgram lblPrelude_
        $ HashMap.fromList
          [ (lblPrelude_, X86Int.MkBlock $ prelude ++ [Jmp lblMain_])
          , (lblMain_, X86Int.MkBlock $ main ++ [Jmp lblConclusion_])
          , (lblConclusion_, X86Int.MkBlock $ conclusion ++ exit)
          ]

compile :: (Error PipelineErr :> es, Gensym :> es) => Platform -> LVar.Expr -> Eff es X86Int.Program
compile platform lvar = do
  lvarmon <- (passRemoveComplexOperands <=< passUniquify) lvar
  let cvar = passExplicateControl lvarmon
  let xvar = passSelectInstructions cvar
  let lt = uncoverLive mempty xvar
  let ig = buildInterference lt
  (xint, frame) <- runState X86Int.emptyFrame (passAllocateRegisters ig xvar)
  let patched = passPatchInstructions xint
  pure $ passGeneratePreludeAndConclusion platform frame.size patched
