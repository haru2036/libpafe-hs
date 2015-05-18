import Bindings.Libpafe.Pasori
import Bindings.Libpafe.Types
import Bindings.Libpafe.Felica
import Foreign.Ptr
import Foreign.Storable
import Foreign.ForeignPtr
import Data.ByteString.Lazy
import Codec.Text.IConv
import Data.Maybe

main :: IO()
main = do
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
      print $ convert "EUCJP" "UTF8" $ pack $ fromJust maybeBlockWord
      pasoriClose pasori
    Nothing -> print "Pasori is not connected"

