module PPU where

import Data.Bits (Bits, (.|.), (.&.), complement)
import Data.Word (Word8, Word16)

screenWidth = 256
screenHeight = 240
cyclesPerScanline = 114
vBlankScanline = 241
lastScanline = 261

data Sprite = Sprite {
    x :: Word8,
    y :: Word8,
    tileIndex :: Word8,
    attribute :: Word8
}

data SpriteSize = Size8x8 | Size8x16

data ScrollDirection = XDirection | YDirection

data Regs = Regs {
    control :: Word8,
    mask    :: Word8,
    status  :: Word8,
    oam     :: Word16, -- FIXME: redo this
    scrollX :: Word8,
    scrollY :: Word8,
    scrollDirection :: ScrollDirection,
    address :: Word16
}

spriteOverflowMask = 0x20
spriteZeroHitMask = 0x40
inVBlankMask = 0x80

getFlag :: (Bits b, Num b) => b -> b -> Bool
getFlag word mask = (word .&. mask) /= 0

setFlag :: (Bits b) => b -> b -> Bool -> b
setFlag word mask val =
    if val
        then word .|. mask
        else word .&. (complement mask)

xScrollOffset regs = if (control regs .&. 0x01) == 0 then 0 else screenWidth :: Word16
yScrollOffset regs = if (control regs .&. 0x02) == 0 then 0 else screenHeight :: Word16
vramAddrIncrement regs = if (control regs .&. 0x04) == 0 then 1 else 32 :: Word16
spritePatternTableAddr regs = if (control regs .&. 0x08) == 0 then 0 else 0x1000 :: Word16
backgroundPatternTableAddr regs = if (control regs .&. 0x10) == 0 then 0 else 0x1000 :: Word16
spriteSize regs = if (control regs .&. 0x20) == 0 then Size8x8 else Size8x16
vBlankNMI regs = (control regs .&. 0x80) /= 0
showBackground regs = (mask regs .&. 0x08) /= 0
showSprites regs = (mask regs .&. 0x10) /= 0
setSpriteOverflow val regs = regs { status = setFlag (status regs) spriteOverflowMask val }
setSpriteZeroHit val regs = regs { status = setFlag (status regs) spriteZeroHitMask val }
setInVBlank val regs = regs { status = setFlag (status regs) inVBlankMask val }

palette :: [(Word8, Word8, Word8)]
palette = [
    (124,124,124),    (0,0,252),        (0,0,188),        (68,40,188),
    (148,0,132),      (168,0,32),       (168,16,0),       (136,20,0),
    (80,48,0),        (0,120,0),        (0,104,0),        (0,88,0),
    (0,64,88),        (0,0,0),          (0,0,0),          (0,0,0),
    (188,188,188),    (0,120,248),      (0,88,248),       (104,68,252),
    (216,0,204),      (228,0,88),       (248,56,0),       (228,92,16),
    (172,124,0),      (0,184,0),        (0,168,0),        (0,168,68),
    (0,136,136),      (0,0,0),          (0,0,0),          (0,0,0),
    (248,248,248),    (60,188,252),     (104,136,252),    (152,120,248),
    (248,120,248),    (248,88,152),     (248,120,88),     (252,160,68),
    (248,184,0),      (184,248,24),     (88,216,84),      (88,248,152),
    (0,232,216),      (120,120,120),    (0,0,0),          (0,0,0),
    (252,252,252),    (164,228,252),    (184,184,248),    (216,184,248),
    (248,184,248),    (248,164,192),    (240,208,176),    (252,224,168),
    (248,216,120),    (216,248,120),    (184,248,184),    (184,248,216),
    (0,252,252),      (248,216,248),    (0,0,0),          (0,0,0)]
