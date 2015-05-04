{-# LANGUAGE ForeignFunctionInterface #-}
module Bindings.Libpafe.Types where

#include "libpafe.h"

import Foreign.Ptr

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

data Pasori

