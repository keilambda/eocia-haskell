module Pipeline (module Pipeline) where

import Data.HashMap.Strict (findWithDefault, insert)

import Core (Name (MkName, getName))
import Gensym (MonadGensym (gensym))
import LVar qualified

passUniquify :: (MonadGensym m) => LVar.Expr -> m LVar.Expr
passUniquify = loop mempty
 where
  loop env = \case
    e@(LVar.Lit _) -> pure e
    LVar.Var n -> pure $ LVar.Var (findWithDefault n n env)
    LVar.Let name expr body -> do
      name' <- MkName <$> gensym (getName name ++ ".")
      expr' <- loop env expr
      body' <- loop (insert name name' env) body
      pure $ LVar.Let name' expr' body'
    LVar.Prim op es -> LVar.Prim op <$> traverse (loop env) es
