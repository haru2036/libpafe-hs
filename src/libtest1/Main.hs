import Bindings.Libpafe.Pasori
import Bindings.Libpafe.Types
import Foreign.Ptr

main :: IO()
main = do
  pasori <- pasori_open
  print pasori
  pasori_close pasori
