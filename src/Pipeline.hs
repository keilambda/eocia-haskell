module Pipeline (module Pipeline) where

import Data.HashMap.Strict (delete, insert)
import Data.Unique (hashUnique, newUnique)

import LVar qualified

rebind :: String -> String -> LVar.Expr -> LVar.Expr
rebind old new = \case
  e@(LVar.Lit _) -> e
  e@(LVar.Var old') -> if old == old' then LVar.Var new else e
  e@(LVar.Let old' expr body) ->
    if old == old'
      then e
      else LVar.Let old' (rebind old new expr) (rebind old new body)
  LVar.Prim op es -> LVar.Prim op (map (rebind old new) es)

passUniquify :: LVar.Env -> LVar.Expr -> IO LVar.Expr
passUniquify env = \case
  e@(LVar.Lit _) -> pure e
  e@(LVar.Var _) -> pure e
  LVar.Let name expr body -> do
    name' <- (\n -> name ++ "." ++ show n) . hashUnique <$> newUnique
    expr' <- passUniquify env expr
    body' <- passUniquify (insert name' expr (delete name env)) (rebind name name' body)
    pure $ LVar.Let name' expr' body'
  LVar.Prim op es -> LVar.Prim op <$> traverse (passUniquify env) es
