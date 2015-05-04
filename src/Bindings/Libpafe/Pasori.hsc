{-# LANGUAGE ForeignFunctionInterface #-}

module Bindings.Libpafe.Pasori where
#include "libpafe.h"
#include "pasori_command.h"

import Foreign.Ptr
import Foreign.C.Types
import Bindings.Libpafe.Types

foreign import ccall pasori_open :: IO (Ptr Pasori)

foreign import ccall pasori_init :: Ptr Pasori -> IO CInt

foreign import ccall pasori_close :: Ptr Pasori -> IO ()

