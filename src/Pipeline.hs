{-# LANGUAGE OverloadedStrings #-}

module Pipeline (module Pipeline) where

import Control.Monad.State.Strict (State, get, modify)

import Data.HashMap.Strict (findWithDefault, insert, (!?))
import Data.Traversable (for)

import Core
import Stage.CVar qualified as CVar
import Stage.LVar qualified as LVar
import Stage.LVarMon qualified as LVarMon
import Stage.X86Int qualified as X86Int
import Stage.X86Var qualified as X86Var

passUniquify :: (MonadGensym m) => LVar.Expr -> m LVar.Expr
passUniquify = loop mempty
 where
  loop env = \case
    e@(LVar.Lit _) -> pure e
    LVar.Var n -> pure $ LVar.Var (findWithDefault n n env)
    LVar.Let name expr body -> do
      name' <- MkName <$> gensym (getName name <> ".")
      expr' <- loop env expr
      body' <- loop (insert name name' env) body
      pure $ LVar.Let name' expr' body'
    e@(LVar.NulApp _) -> pure e
    LVar.UnApp op a -> LVar.UnApp op <$> loop env a
    LVar.BinApp op a b -> LVar.BinApp op <$> loop env a <*> loop env b

{-# SPECIALISE passUniquify :: LVar.Expr -> State Int LVar.Expr #-}

passRemoveComplexOperands :: (MonadGensym m) => LVar.Expr -> m LVarMon.Expr
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
  genTmpName :: (MonadGensym m) => m Name
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

{-# SPECIALISE passRemoveComplexOperands :: LVar.Expr -> State Int LVarMon.Expr #-}

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

passSelectInstructions :: CVar.Tail -> X86Var.Block
passSelectInstructions = \case
  CVar.Return (CVar.Atom atom) -> X86Var.MkBlock [MovQ (fromAtom atom) (X86Var.Reg RAX), Jmp "conclusion"]
  CVar.Return (CVar.NulApp op) -> X86Var.MkBlock $ fromNulOp op ++ [Jmp "conclusion"]
  CVar.Return (CVar.UnApp op a) -> X86Var.MkBlock $ fromUnOp op a ++ [Jmp "conclusion"]
  CVar.Return (CVar.BinApp op a b) -> X86Var.MkBlock $ fromBinOp op a b ++ [Jmp "conclusion"]
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
    CVar.BinApp op a b | cname /= a && cname /= b -> case op of
      Add ->
        [ MovQ (fromAtom a) (X86Var.Var name)
        , AddQ (fromAtom b) (X86Var.Var name)
        ]
      Sub ->
        [ MovQ (fromAtom a) (X86Var.Var name)
        , SubQ (fromAtom b) (X86Var.Var name)
        ]
     where
      cname = Var name
    CVar.BinApp op a b -> fromBinOp op a b ++ rax2Var name

passAssignHomes :: X86Var.Block -> State X86Int.Frame X86Int.Block
passAssignHomes (X86Var.MkBlock xs) =
  X86Int.MkBlock <$> for xs \case
    AddQ src tgt -> AddQ <$> spill src <*> spill tgt
    SubQ src tgt -> SubQ <$> spill src <*> spill tgt
    NegQ tgt -> NegQ <$> spill tgt
    MovQ src tgt -> MovQ <$> spill src <*> spill tgt
    PushQ tgt -> PushQ <$> spill tgt
    PopQ tgt -> PopQ <$> spill tgt
    CallQ lbl n -> pure (CallQ lbl n)
    Jmp lbl -> pure (Jmp lbl)
    Syscall -> pure Syscall
    RetQ -> pure RetQ
 where
  spill :: X86Var.Arg -> State X86Int.Frame X86Int.Arg
  spill = \case
    X86Var.Imm n -> pure (X86Int.Imm n)
    X86Var.Reg reg -> pure (X86Int.Reg reg)
    X86Var.Deref n reg -> pure (X86Int.Deref n reg)
    X86Var.Var name -> do
      X86Int.MkFrame{env, offset} <- get
      case env !? name of
        Just arg -> pure arg
        Nothing -> do
          let offset' = offset - 8
              arg = X86Int.Deref offset' RBP
          modify \s -> s{X86Int.env = insert name arg env, X86Int.offset = offset'}
          pure arg
