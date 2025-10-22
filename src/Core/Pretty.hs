module Core.Pretty
  ( renderText
  , putPrettyLn
  ) where

import Data.Text.IO qualified as Text
import Pre
import Prettyprinter.Render.Text (renderStrict)

renderText :: (Pretty a) => a -> Text
renderText = renderStrict . layoutPretty defaultLayoutOptions . pretty

putPrettyLn :: (Pretty a) => a -> IO ()
putPrettyLn = Text.putStrLn . renderText
