{-# LANGUAGE ForeignFunctionInterface #-}
module Bindings.Libpafe.Felica(
  felica_polling
 ,felicaRead
 ,felicaReadSingle
) where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Bindings.Libpafe.Types
import Bindings.Libpafe.Pasori

#include <libpafe/libpafe.h>
#include <libpafe/felica_command.h>

foreign import ccall felica_polling :: Ptr Pasori -> CUInt16 -> CUInt8 -> CUInt8 -> IO (Ptr Felica)

--int felica_read(felica * f, int *n, felica_block_info *info, uint8 *data);
foreign import ccall felica_read :: Ptr Felica -> Ptr Int -> Ptr FelicaBlockInfo -> Ptr CUInt8 -> IO Int

-- int felica_read_single(felica * f, int servicecode, int mode, uint8 block, uint8 *data);
foreign import ccall felica_read_single :: Ptr Felica -> Int -> Int -> CUInt8 -> Ptr CUInt8 -> IO Int

felicaRead :: Ptr Felica -> Int -> FelicaBlockInfo -> IO (Maybe [CUInt8])
felicaRead felica bufferLength blockInfo = do
  bufferLenPtr <- malloc 
  blockInfoPtr <- malloc
  _ <- poke blockInfoPtr blockInfo 
  _ <- poke bufferLenPtr bufferLength
  bufferPtr <- mallocForeignPtrArray bufferLength
  result <- withForeignPtr bufferPtr $ felica_read felica bufferLenPtr blockInfoPtr
  free bufferLenPtr
  free blockInfoPtr
  case result of
    0 -> do
      resultValue <- withForeignPtr bufferPtr (peekArray bufferLength)
      return $ Just resultValue
    err -> print err >> return Nothing

felicaReadSingle :: Ptr Felica -> Int -> Int -> CUInt8 -> IO (Maybe [CUInt8])
felicaReadSingle felica mode servCode blk = do
  bufferPtr <- mallocForeignPtrArray 8
  result <- withForeignPtr bufferPtr $ felica_read_single felica servCode mode blk 
  case result of
    0 -> do
      resultvValue <- withForeignPtr bufferPtr (peekArray 8)
      return $ Just resultvValue
    err -> return Nothing


