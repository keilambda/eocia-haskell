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
  deriving stock (Eq, Show)

interpExpr :: (MonadError LVarErr m, MonadIO m, MonadReader Env m) => Expr -> m Int
interpExpr = \case
  Lit n -> pure n
  Var n -> do
    env <- ask
    maybe (throwError (UnboundVariable n)) interpExpr (HashMap.lookup n env)
  Let n e body -> do
    e' <- interpExpr e
    local (HashMap.insert n (Lit e')) $ interpExpr body
  NulApp Read -> do
    str <- liftIO Text.getLine
    case Text.decimal str of
      Right (r, Empty) -> pure r
      _ -> throwError (InvalidReadInput str)
  UnApp Neg a -> negate <$> interpExpr a
  BinApp Add a b -> (+) <$> interpExpr a <*> interpExpr b
  BinApp Sub a b -> (-) <$> interpExpr a <*> interpExpr b

runInterpExpr :: Expr -> IO (Either LVarErr Int)
runInterpExpr e = runExceptT (runReaderT (interpExpr e) mempty)
