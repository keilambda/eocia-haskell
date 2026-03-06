{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Core
import Data.Maybe
import Data.Text.IO qualified as Text
import Effectful.Error.Static
import Effectful.FileSystem
import Pipeline
import Pre
import Stage.LVar qualified as LVar
import System.IO (hPutStr, stderr)

main :: IO ()
main = do
  let expr = LVar.Let "x" (LVar.Lit 32) (LVar.BinApp Add (LVar.Var "x") (LVar.NulApp Read))
  let platform = fromJust hostPlatform
  let res = runPureEff . runErrorNoCallStack @PipelineErr . runGensymPure @Int 0 $ fullPipeline platform expr
  case res of
    Left err -> hPutStr stderr $ show err
    Right asm -> Text.putStrLn asm

fullPipeline :: (Error PipelineErr :> es, Gensym :> es) => Platform -> LVar.Expr -> Eff es Text
fullPipeline platform code = do
  renderText <$> compile platform code
