module LVar (module LVar) where

import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (liftIO)

import Data.HashMap.Strict (HashMap, insert, (!?))
import Data.Kind (Type)
import Data.List (List)

import LInt (Op (..))

type Expr :: Type
data Expr
  = Lit Int
  | Var String
  | Let String Expr Expr
  | Prim Op (List Expr)
  deriving stock (Show)

read_ :: Expr
read_ = Prim Read []

neg :: Expr -> Expr
neg e = Prim Neg [e]

add :: Expr -> Expr -> Expr
add a b = Prim Add [a, b]

sub :: Expr -> Expr -> Expr
sub a b = Prim Sub [a, b]

type Env :: Type
type Env = HashMap String Expr

type LVarErr :: Type
data LVarErr
  = UnboundVariable String
  | BadSpecialForm Expr
  deriving stock (Show)

interpExpr :: Env -> Expr -> ExceptT LVarErr IO Int
interpExpr env = \case
  Lit n -> pure n
  Var n -> maybe (throwError (UnboundVariable n)) (interpExpr env) (env !? n)
  Let n e body -> interpExpr (insert n e env) body
  s@(Prim n xs) -> case (n, xs) of
    (Read, []) -> read <$> liftIO getLine
    (Neg, [a]) -> negate <$> (interpExpr env a)
    (Add, [a, b]) -> (+) <$> (interpExpr env a) <*> (interpExpr env b)
    (Sub, [a, b]) -> (-) <$> (interpExpr env a) <*> (interpExpr env b)
    _ -> throwError (BadSpecialForm s)

runInterpExpr :: Expr -> IO (Either LVarErr Int)
runInterpExpr e = runExceptT (interpExpr mempty e)
