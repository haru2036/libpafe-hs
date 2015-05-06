import Bindings.Libpafe.Pasori
import Bindings.Libpafe.Types
import Foreign.Ptr

main :: IO()
main = do
  maybePasori <- preparePasori
  case maybePasori of
    Just pasori -> do
      print $ pasori
      pasoriClose pasori
    Nothing -> print "pasori is nothing"
