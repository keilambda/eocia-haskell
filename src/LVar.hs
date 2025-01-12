{-# LANGUAGE PatternSynonyms #-}

module LVar (module LVar) where

import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (liftIO)

import Data.HashMap.Strict (HashMap, insert, (!?))
import Data.Kind (Type)
import Data.List (List)
import Data.Text (Text, pattern Empty)
import Data.Text.IO qualified as TIO
import Data.Text.Read (decimal)

import Prettyprinter

import Core (Name)
import LInt (Op (..))

type Expr :: Type
data Expr
  = Lit Int
  | Var Name
  | Let Name Expr Expr
  | Prim Op (List Expr)
  deriving stock (Show)

instance Pretty Expr where
  pretty = \case
    Lit n -> pretty n
    Var n -> pretty n
    Let n e body -> parens $ pretty "let" <+> brackets (pretty n <+> pretty e) <+> pretty body
    Prim op es -> parens $ pretty op <+> hsep (map pretty es)

read_ :: Expr
read_ = Prim Read []

neg :: Expr -> Expr
neg e = Prim Neg [e]

add :: Expr -> Expr -> Expr
add a b = Prim Add [a, b]

sub :: Expr -> Expr -> Expr
sub a b = Prim Sub [a, b]

type Env :: Type
type Env = HashMap Name Expr

type LVarErr :: Type
data LVarErr
  = UnboundVariable Name
  | BadSpecialForm Expr
  | InvalidReadInput Text
  deriving stock (Show)

interpExpr :: Env -> Expr -> ExceptT LVarErr IO Int
interpExpr env = \case
  Lit n -> pure n
  Var n -> maybe (throwError (UnboundVariable n)) (interpExpr env) (env !? n)
  Let n e body -> do
    e' <- interpExpr env e
    interpExpr (insert n (Lit e') env) body
  s@(Prim n xs) -> case (n, xs) of
    (Read, []) -> do
      str <- liftIO TIO.getLine
      case decimal str of
        Right (r, Empty) -> pure r
        _ -> throwError (InvalidReadInput str)
    (Neg, [a]) -> negate <$> (interpExpr env a)
    (Add, [a, b]) -> (+) <$> (interpExpr env a) <*> (interpExpr env b)
    (Sub, [a, b]) -> (-) <$> (interpExpr env a) <*> (interpExpr env b)
    _ -> throwError (BadSpecialForm s)

runInterpExpr :: Expr -> IO (Either LVarErr Int)
runInterpExpr e = runExceptT (interpExpr mempty e)
