{-# LANGUAGE ForeignFunctionInterface #-}
module Bindings.Libpafe.Pasori (
  pasoriPrepare
 ,pasoriClose
 ,withPasori
) where

#include <libpafe/libpafe.h>
#include <libpafe/pasori_command.h>

import Foreign.Ptr
import Foreign.C.Types
import Bindings.Libpafe.Types
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class

pasoriPrepare :: IO (Maybe (Ptr Pasori))
pasoriPrepare = do
  pasori <- pasori_open
  result <- pasori_init pasori
  case result of
    0 -> return $ Just pasori
    otherwise -> pasoriClose pasori >> return Nothing

withPasori :: (Ptr Pasori -> IO a) -> IO (Maybe a)
withPasori act = runMaybeT $ do
  pasori <- MaybeT pasoriPrepare
  result <- liftIO $ act pasori
  liftIO $ pasoriClose pasori
  return result

pasoriClose :: Ptr Pasori -> IO() 
pasoriClose = pasori_close

foreign import ccall pasori_open :: IO (Ptr Pasori)

foreign import ccall pasori_init :: Ptr Pasori -> IO CInt

foreign import ccall pasori_close :: Ptr Pasori -> IO ()

