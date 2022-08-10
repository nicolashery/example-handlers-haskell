module App.Text (tshow, tlshow) where

import Data.Text (Text, pack)
import Data.Text.Lazy qualified as TL

tshow :: (Show a) => a -> Text
tshow v = pack $ show v

tlshow :: (Show a) => a -> TL.Text
tlshow v = TL.pack $ show v
