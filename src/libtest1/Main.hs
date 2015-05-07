import Bindings.Libpafe.Pasori
import Bindings.Libpafe.Types
import Bindings.Libpafe.Felica
import Foreign.Ptr

main :: IO()
main = do
  maybePasori <- pasoriPrepare
  case maybePasori of
    Just pasori -> do
      felica <- felica_polling pasori 0xffff 0 0 
      print felica
      pasoriClose pasori
    Nothing -> print "pasori is nothing"
