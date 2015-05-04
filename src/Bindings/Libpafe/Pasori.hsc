{-# LANGUAGE ForeignFunctionInterface #-}

module Bindings.Libpafe.Pasori where
#include "pasori_command.h"
#include "libpafe.h"

import Foreign.Ptr
import Foreign.C.Types
import Bindings.Libpafe.Types

foreign import ccall pasori_open :: IO (Ptr Pasori)

foreign import ccall pasori_init :: Ptr Pasori -> IO CInt

foreign import ccall pasori_close :: Ptr Pasori -> IO ()

