module Main where

import Bindings.Libpafe.Pasori
import Bindings.Libpafe.Types
import Bindings.Libpafe.Felica
import Prelude hiding (splitAt)
import Foreign.Ptr
import Foreign.Storable
import Foreign.ForeignPtr
import Data.ByteString.Lazy
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Maybe
import Codec.Text.IConv
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Control.Monad

import System.Exit (exitFailure)

main :: IO()
main = do
  felicaStr <- withPasori $ runMaybeT . readFelica
  print felicaStr
  print $ fromJust $ join felicaStr

readFelica :: Ptr Pasori -> MaybeT IO String
readFelica pasori = do
  liftIO $ print pasori
  felicaPtr <- MaybeT $ felicaPolling 0xffff 0 0 pasori 
  liftIO $ print felicaPtr
  felica <- liftIO $ withForeignPtr felicaPtr peek 
  return $ show felica

