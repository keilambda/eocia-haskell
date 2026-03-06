{-# LANGUAGE PatternSynonyms #-}

module Stage.LVar (module Stage.LVar) where

import Core (BinOp (..), Literal (..), Name, NulOp (..), UnOp (..), abool, aint)
import Core.Input
import Data.HashMap.Strict qualified as HashMap
import Data.Text (pattern Empty)
import Data.Text.Read qualified as Text
import Effectful.Error.Static
import Effectful.Reader.Static
import Pre

type Expr :: Type
data Expr
  = Lit Literal
  | Var Name
  | Let Name Expr Expr
  | NulApp NulOp
  | UnApp UnOp Expr
  | BinApp BinOp Expr Expr
  deriving stock (Eq, Show)

instance Pretty Expr where
  pretty = \case
    Lit n -> pretty n
    Var n -> pretty n
    Let n e body -> parens $ pretty "let" <+> brackets (pretty n <+> pretty e) <+> pretty body
    NulApp op -> parens $ pretty op
    UnApp op a -> parens $ pretty op <+> pretty a
    BinApp op a b -> parens $ pretty op <+> pretty a <+> pretty b

int :: Int -> Expr
int = Lit . LInt

bool :: Bool -> Expr
bool = Lit . LBool

read_ :: Expr
read_ = NulApp Read

neg :: Expr -> Expr
neg = UnApp Neg

add :: Expr -> Expr -> Expr
add = BinApp Add

sub :: Expr -> Expr -> Expr
sub = BinApp Sub

type Env :: Type
type Env = HashMap Name Expr

type LVarErr :: Type
data LVarErr
  = UnboundVariable Name
  | InvalidReadInput Text
  | MismatchedType Text
  deriving stock (Eq, Show)

checkInt :: (Error LVarErr :> es) => Literal -> Eff es Int
checkInt = \case
  LInt a -> pure a
  _ -> throwError (MismatchedType "expected Integer")

checkBool :: (Error LVarErr :> es) => Literal -> Eff es Bool
checkBool = \case
  LBool a -> pure a
  _ -> throwError (MismatchedType "expected Boolean")

-- TODO: currently a literal is the same as a value, but when we add closures, we need to rethink the return type
interpExpr :: (Error LVarErr :> es, Input :> es, Reader Env :> es) => Expr -> Eff es Literal
interpExpr = \case
  Lit n -> pure n
  Var n -> do
    env <- ask
    maybe (throwError (UnboundVariable n)) interpExpr (HashMap.lookup n env)
  Let n e body -> do
    e' <- interpExpr e
    local (HashMap.insert n (Lit e')) $ interpExpr body
  NulApp Read -> do
    str <- readLine
    case Text.decimal str of
      Right (r, Empty) -> pure (LInt r)
      _ -> case str of
        "#f" -> pure (LBool False)
        "#t" -> pure (LBool True)
        _ -> throwError (InvalidReadInput str)
  UnApp Neg a -> LInt . negate <$> (checkInt =<< interpExpr a)
  BinApp Add a b -> fmap LInt $ (+) <$> (checkInt =<< interpExpr a) <*> (checkInt =<< interpExpr b)
  BinApp Sub a b -> fmap LInt $ (-) <$> (checkInt =<< interpExpr a) <*> (checkInt =<< interpExpr b)

runInterpExprConst :: Text -> Expr -> Either LVarErr Literal
runInterpExprConst text = runPureEff . runInputConst text . runErrorNoCallStack . runReader mempty . interpExpr

runInterpExprIO :: Expr -> IO (Either LVarErr Literal)
runInterpExprIO = runEff . runInputStdin . runErrorNoCallStack . runReader mempty . interpExpr
