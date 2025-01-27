module Core.Pretty
  ( renderText
  , putPrettyLn
  ) where

import Data.Text (Text)
import Data.Text.IO qualified as TIO

import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)

renderText :: (Pretty a) => a -> Text
renderText = renderStrict . layoutPretty defaultLayoutOptions . pretty

putPrettyLn :: (Pretty a) => a -> IO ()
putPrettyLn = TIO.putStrLn . renderText
