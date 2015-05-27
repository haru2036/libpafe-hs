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

readPasori :: IO ByteString
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
      maybeBlockWord <- withForeignPtr felicaPtr $ felicaReadSingle 0 0x1A8B 1
      maybeBlockWord2 <- withForeignPtr felicaPtr $ felicaReadSingle 0 0x1A8B 0
      let cardId = fcfId $ convert "EUCJP" "UTF8" $ pack $ fromJust maybeBlockWord2
      pasoriClose pasori
      return cardId
    Nothing -> error "Pasori is not connected"

fcfId = snd . splitAt 2 . fst . splitAt 9
