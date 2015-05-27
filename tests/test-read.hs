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

import System.Exit (exitFailure)

main = readPasori >>= print

readPasori :: IO String
readPasori = do
  maybePasori <- pasoriPrepare
  case maybePasori of
    Just pasori -> do
      maybeFelicaPtr <- felicaPolling 0xfe00 0 0 pasori 
      let felicaPtr = fromJust maybeFelicaPtr
      felica <- withForeignPtr felicaPtr peek 
      print "IDm is:"
      print felica
      print "PMm is:"
      print $ pmm felica
      pasoriClose pasori
      return $ show felica
    Nothing -> error "Pasori is not connected"

