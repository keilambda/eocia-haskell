{-# LANGUAGE PatternSynonyms #-}

module Stage.LVar (module Stage.LVar) where

import Core (BinOp (..), Name, NulOp (..), UnOp (..))
import Data.HashMap.Strict qualified as HashMap
import Data.Text (pattern Empty)
import Data.Text.IO qualified as Text
import Data.Text.Read qualified as Text
import Pre

type Expr :: Type
data Expr
  = Lit Int
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

read_ :: Expr
read_ = NulApp Read

neg :: Expr -> Expr
neg e = UnApp Neg e

add :: Expr -> Expr -> Expr
add a b = BinApp Add a b

sub :: Expr -> Expr -> Expr
sub a b = BinApp Sub a b

type Env :: Type
type Env = HashMap Name Expr

type LVarErr :: Type
data LVarErr
  = UnboundVariable Name
  | InvalidReadInput Text
  deriving stock (Eq, Show)

interpExpr :: Env -> Expr -> ExceptT LVarErr IO Int
interpExpr env = \case
  Lit n -> pure n
  Var n -> maybe (throwError (UnboundVariable n)) (interpExpr env) (HashMap.lookup n env)
  Let n e body -> do
    e' <- interpExpr env e
    interpExpr (HashMap.insert n (Lit e') env) body
  NulApp Read -> do
    str <- liftIO Text.getLine
    case Text.decimal str of
      Right (r, Empty) -> pure r
      _ -> throwError (InvalidReadInput str)
  UnApp Neg a -> negate <$> interpExpr env a
  BinApp Add a b -> (+) <$> interpExpr env a <*> interpExpr env b
  BinApp Sub a b -> (-) <$> interpExpr env a <*> interpExpr env b

runInterpExpr :: Expr -> IO (Either LVarErr Int)
runInterpExpr e = runExceptT (interpExpr mempty e)
