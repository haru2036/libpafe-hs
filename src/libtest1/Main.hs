import Bindings.Libpafe.Pasori
import Bindings.Libpafe.Types
import Bindings.Libpafe.Felica
import Foreign.Ptr
import Foreign.Storable
import Data.ByteString.Lazy
import Codec.Text.IConv
import Data.Maybe

main :: IO()
main = do
  maybePasori <- pasoriPrepare
  case maybePasori of
    Just pasori -> do
      felicaPtr <- felica_polling pasori 0xfe00 0 0 
      felica <- peek felicaPtr
      print "IDm is:"
      print felica
      print "PMm is:"
      print $ pmm felica
      maybeBlockWord <- felicaReadSingle felicaPtr 0 0x1A8B 1
      print $ convert "EUCJP" "UTF8" $ pack $ fromJust maybeBlockWord
      {-
      print "Area Number is:"
      print $ areaNum felica
      print "Area is:"
      print $ felicaArea felica
      print "ServiceNumber is:"
      print $ serviceNum felica
      print "Service is:"
      print $ service felica
      print "NextFelica Ptr is:"
      print $ nextFelica felica
      -}
      pasoriClose pasori
    Nothing -> print "pasori is nothing"

