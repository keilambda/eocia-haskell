{-# LANGUAGE PatternSynonyms #-}

module Stage.LVar (module Stage.LVar) where

import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (liftIO)

import Data.HashMap.Strict (HashMap, insert, (!?))
import Data.Kind (Type)
import Data.Text (Text, pattern Empty)
import Data.Text.IO qualified as TIO
import Data.Text.Read (decimal)

import Prettyprinter

import Core (BinOp (..), Name, NulOp (..), UnOp (..))

type Expr :: Type
data Expr
  = Lit Int
  | Var Name
  | Let Name Expr Expr
  | NulApp NulOp
  | UnApp UnOp Expr
  | BinApp BinOp Expr Expr
  deriving stock (Show, Eq)

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
  deriving stock (Show, Eq)

interpExpr :: Env -> Expr -> ExceptT LVarErr IO Int
interpExpr env = \case
  Lit n -> pure n
  Var n -> maybe (throwError (UnboundVariable n)) (interpExpr env) (env !? n)
  Let n e body -> do
    e' <- interpExpr env e
    interpExpr (insert n (Lit e') env) body
  NulApp Read -> do
    str <- liftIO TIO.getLine
    case decimal str of
      Right (r, Empty) -> pure r
      _ -> throwError (InvalidReadInput str)
  UnApp Neg a -> negate <$> (interpExpr env a)
  BinApp Add a b -> (+) <$> (interpExpr env a) <*> (interpExpr env b)
  BinApp Sub a b -> (-) <$> (interpExpr env a) <*> (interpExpr env b)

runInterpExpr :: Expr -> IO (Either LVarErr Int)
runInterpExpr e = runExceptT (interpExpr mempty e)
