module ROM where

import Data.Bits ((.|.), (.&.), shiftL, shiftR)
import Data.Word (Word8, Word16)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Attoparsec.ByteString (word8, anyWord8, string)
import Data.Attoparsec.ByteString as A
import qualified Memory as M

data Mirroring = Horizontal | Vertical | FourScreen deriving (Eq, Show)

data Region = NTSC | PAL deriving (Eq, Show)

data ROM = ROM {
    mirroring  :: Mirroring,
    trainer    :: Bool,
    persistent :: Bool,
    inesMapper :: Word8,
    mapper     :: Word8,
    playChoice :: Bool,
    unisystem  :: Bool,
    ramSize    :: Word16,
    region     :: Region,
    prg        :: ByteString,
    chr        :: ByteString
} deriving (Eq, Show)

pROM = do
    -- Header is 16 bytes
    string (B.pack [0x4e, 0x45, 0x53, 0x1a]) -- "NES\x1a"
    prgSize16KB <- anyWord8
    chrSize8KB  <- anyWord8
    flags6      <- anyWord8
    flags7      <- anyWord8
    ramSize8KB  <- anyWord8
    flags9      <- anyWord8
    _           <- anyWord8
    count 5 (word8 0x00)

    prgBytes <- A.take (fromIntegral prgSize16KB `shiftL` 14)
    chrBytes <- A.take (fromIntegral chrSize8KB `shiftL` 13)

    return $ ROM {
        mirroring = if (flags6 .&. 0x08) /= 0 then FourScreen else
                    if (flags6 .&. 0x01) /= 0 then Vertical else Horizontal,
        trainer    = (flags6 .&. 0x04) /= 0,
        persistent = (flags6 .&. 0x02) /= 0,
        inesMapper =  flags6 `shiftR` 4,
        mapper     = (flags7 .&. 0xf0) .|. (flags6 `shiftR` 4),
        playChoice = (flags7 .&. 0x02) /= 0,
        unisystem  = (flags7 .&. 0x01) /= 0,
        ramSize    = M.byteToWord ramSize8KB `shiftL` 13,
        region     = if (flags9 .&. 0x01) == 0 then NTSC else PAL,
        prg = prgBytes,
        chr = chrBytes
    }

parseROM :: ByteString -> Either String ROM
parseROM = parseOnly pROM
