{-# LANGUAGE ForeignFunctionInterface #-}
module Bindings.Libpafe.Types (
  Pasori
 ,Felica
 ,CUInt8
 ,CUInt16
)
where

#include <libpafe/libpafe.h>

import Foreign.Ptr
import Foreign.C.Types

{-
data FelicaBlockInfo = FelicaBlockInfo {
                                        service :: UINT 
                                        ,mode :: UINT
                                        ,block :: UINT 
                                        }
data FelicaBlock = FelicaBlock { 
                                blockData :: [CUInt]
                                }

#def typedef struct _felica_block_info FelicaBlockInfo;

#def typedef struct felica_block FelicaBlock;
-}

#def typedef uint8 CUInt8;
#def typedef uint16 CUInt16;

type CUInt8 = CChar
type CUInt16 = CShort

data Pasori
data Felica

