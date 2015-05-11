{-# LANGUAGE ForeignFunctionInterface #-}
module Bindings.Libpafe.Types
where

#include <libpafe/libpafe.h>

import Foreign.Ptr
import Foreign.C.Types
import Foreign.Storable
import Foreign.Marshal.Array
import Control.Monad.Reader
import Data.Word

{-
data FelicaBlockInfo = FelicaBlockInfo {
                                        service :: CUInt16
                                        ,mode :: CUInt8
                                        ,block :: CUInt16
                                       }deriving (Storable)

-}
data FelicaBlock = FelicaBlock { 
                                blockData :: [CUInt8]
                               }

#def typedef struct _felica_block_info FelicaBlockInfo;

#def typedef struct felica_block FelicaBlock;

#def typedef uint8 CUInt8;

#def typedef uint16 CUInt16;

#def typedef struct felica_area Area;

type CUInt8 = Word8
type CUInt16 = Word16

data Pasori
data Felica = Felica {
                p :: Ptr Pasori
               ,systemCode :: CUInt16
               ,idm :: [CUInt8]
               ,pmm :: [CUInt8]
               ,areaNum :: CUInt16
               ,felicaArea :: [Area]
               ,serviceNum:: CUInt16
               ,service :: [Area]
               ,nextFelica :: Ptr Felica
               }

data Area = Area {
            code :: CUInt16
           ,attr :: CUInt16
           ,bin :: CUInt16
           ,nextArea :: Ptr Area
           }

instance Storable Felica where
  sizeOf x = #size felica
  alignment = sizeOf
  peek ptr = do 
     p <- (#peek felica, p) ptr
     sc <- (#peek felica, systemcode) ptr
     idm <- peekArray 8 $ (#ptr felica, IDm) ptr
     pmm <- peekArray 8 $ (#ptr felica, PMm) ptr
     an <- (#peek felica, area_num) ptr
     fa <- peekArray  256 $ (#ptr felica, area) ptr
     sn <- (#peek felica, service_num) ptr
     s <- peekArray  256 $ (#ptr felica, service) ptr
     n <- (#peek felica, next) ptr
     return $ Felica p sc idm pmm an fa sn s n
  poke ptr (Felica p sc idm pmm an fa sn s n) = do
    (#poke felica, p) ptr p
    (#poke felica, systemcode) ptr sc
    pokeArray ((#ptr felica, IDm) ptr) idm
    pokeArray ((#ptr felica, PMm) ptr) pmm
    (#poke felica, area_num) ptr an
    pokeArray ((#ptr felica, area) ptr) fa
    (#poke felica, service_num) ptr sn
    pokeArray ((#ptr felica, service) ptr) s
    (#poke felica, next) ptr n

instance Storable Area where
  sizeOf x = #size felica_area
  alignment = sizeOf
  peek ptr = do
    cde <- (#peek felica_area, code) ptr 
    attrb <- (#peek felica_area, attr) ptr 
    binary <- (#peek felica_area, bin) ptr 
    nextarea <- (#peek felica_area, next) ptr 
    return $ Area cde attrb binary nextarea
  poke ptr (Area cde attrib binary nextarea) = do
    (#poke felica_area, code) ptr cde
    (#poke felica_area, attr) ptr attrib
    (#poke felica_area, bin) ptr binary
    (#poke felica_area, next) ptr nextarea

instance Show Felica where
  show = show . idm

instance Show Area where
  show = show . code
