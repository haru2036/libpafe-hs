{-# LANGUAGE ForeignFunctionInterface #-}
module Bindings.Libpafe.Felica(
  felica_polling
) where

import Foreign.Ptr
import Foreign.C.Types
import Bindings.Libpafe.Types

#include <libpafe/libpafe.h>
#include <libpafe/felica_command.h>

foreign import ccall felica_polling :: Ptr Pasori -> CUInt16 -> CUInt8 -> CUInt8 -> IO (Ptr Felica)

