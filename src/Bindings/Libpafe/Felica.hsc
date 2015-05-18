{-# LANGUAGE ForeignFunctionInterface #-}
module Bindings.Libpafe.Felica(
  felicaPolling
 ,felicaReadSingle
 ,justReslts
) where
import Prelude hiding (sequence)

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Data.Traversable
import Bindings.Libpafe.Types
import Bindings.Libpafe.Pasori

#include <libpafe/libpafe.h>
#include <libpafe/felica_command.h>

foreign import ccall felica_polling :: Ptr Pasori -> CUInt16 -> CUInt8 -> CUInt8 -> IO (Ptr Felica)

--int felica_read(felica * f, int *n, felica_block_info *info, uint8 *data);
foreign import ccall felica_read :: Ptr Felica -> Ptr Int -> Ptr FelicaBlockInfo -> Ptr CUInt8 -> IO Int

-- int felica_read_single(felica * f, int servicecode, int mode, uint8 block, uint8 *data);
foreign import ccall felica_read_single :: Ptr Felica -> Int -> Int -> CUInt8 -> Ptr CUInt8 -> IO Int


felicaPolling :: CUInt16 -- ^ ServiceCode
  -> CUInt8 -- ^ RFU (is normally 0)
  -> CUInt8 -- ^ Timeslot
  -> Ptr Pasori  -- ^ Pointer for Pasori
  -> IO (Maybe (ForeignPtr Felica))
felicaPolling sc rfu timeslot p = felica_polling p sc rfu timeslot >>= b

a :: Ptr Felica -> Maybe (IO (ForeignPtr Felica))
a x = nullToNothing x >>= return . freeForeignPtr  

b :: Ptr Felica -> IO (Maybe (ForeignPtr Felica))
b = sequence . a 

nullToNothing :: Ptr Felica -> Maybe (Ptr Felica)
nullToNothing ptr 
  |nullPtr == ptr = Nothing 
  |otherwise = Just ptr

freeForeignPtr :: Ptr a -> IO (ForeignPtr a)
freeForeignPtr = newForeignPtr finalizerFree

felicaReadSingle :: Int -- ^ Felica reading mode
  -> Int -- ^ Service code of block
  -> CUInt8  -- ^ Block number
  -> Ptr Felica -- ^ A pointer for Felica
  -> IO (Maybe [CUInt8])
felicaReadSingle mode servCode blk felica = do
  bufferPtr <- mallocForeignPtrArray 8
  result <- withForeignPtr bufferPtr $ felica_read_single felica servCode mode blk 
  case result of
    0 -> do
      resultvValue <- withForeignPtr bufferPtr (peekArray 16)
      return $ Just resultvValue
    err -> return Nothing


concatMaybe :: [a] -> Maybe [a] -> [a]
concatMaybe acc (Just x) = acc ++ x
concatMaybe acc Nothing  = acc

justReslts = foldl concatMaybe [] 
