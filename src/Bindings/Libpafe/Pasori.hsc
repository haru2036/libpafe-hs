{-# LANGUAGE ForeignFunctionInterface #-}

module Bindings.Libpafe.Pasori (
  preparePasori
 ,pasoriClose
) where
#include <libpafe/libpafe.h>
#include <libpafe/pasori_command.h>

import Foreign.Ptr
import Foreign.C.Types
import Bindings.Libpafe.Types

preparePasori :: IO (Maybe (Ptr Pasori))
preparePasori = do
  pasori <- pasori_open
  result <- pasori_init pasori
  case result of
    0 -> return $ Just pasori
    otherwise -> pasoriClose pasori >> return Nothing

pasoriClose = pasori_close

foreign import ccall pasori_open :: IO (Ptr Pasori)

foreign import ccall pasori_init :: Ptr Pasori -> IO CInt

foreign import ccall pasori_close :: Ptr Pasori -> IO ()

