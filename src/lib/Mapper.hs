module Mapper where

import Data.Bits ((.&.))
import Data.Sequence (Seq)
import Data.Sequence as S
import Data.Word (Word8, Word16)
import Memory (Storage(..))
import ROM (ROM(..))

data MapperResult = Continue | Irq

class Mapper m where
    loadPrgByte :: Word16 -> m -> (Word8, m)
    loadChrByte :: Word16 -> m -> (Word8, m)
    storePrgByte :: Word16 -> Word8 -> m -> m
    storeChrByte :: Word16 -> Word8 -> m -> m
    nextScanline :: m -> MapperResult

data NROM = NROM ROM

instance Mapper NROM where
    loadPrgByte addr (NROM rom) = (result, NROM rom)
        where result
                | addr < 0x8000 =
                    0x08
                | S.length (prg rom) > 16384 =
                    fst $ loadByte (addr .&. 0x7fff) (prg rom)
                | otherwise =
                    fst $ loadByte (addr .&. 0x3fff) (prg rom)
    loadChrByte addr rom = error "loadByte addr (chr rom)"
    storePrgByte = error "Can't write to PRG ROM"
    storeChrByte = error "Can't write to CHR ROM"
    nextScanline _ = Continue
