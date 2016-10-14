module PPU where

import Data.Bits (Bits, (.|.), (.&.), complement)
import Data.Word (Word8, Word16)
import Memory

screenWidth = 256
screenHeight = 240
cyclesPerScanline = 114
vBlankScanline = 241
lastScanline = 261

data Sprite = Sprite {
    xPosition :: Word8,
    yPosition :: Word8,
    tileIndex :: Word8,
    attribute :: Word8
}

data SpriteSize = Size8x8 | Size8x16
data SpriteTile = Tile8x8 Word16 | Tile8x16 Word16 Word16
data SpritePriority = AboveBackground | BelowBackground
data ScrollDirection = XDirection | YDirection
type RGB = (Word8, Word8, Word8)
data PixelLayer = BackgroundLayer | SpriteLayer
type SpriteColor = (SpritePriority, RGB)

data NametableAddress = NametableAddress {
    xIndex :: Word8,
    yIndex :: Word8,
    base :: Word16
}

data Regs = Regs {
    control :: Word8,
    mask    :: Word8,
    status  :: Word8,
    oamAddr :: Word16,
    scrollX :: Word8,
    scrollY :: Word8,
    scrollDirection :: ScrollDirection,
    address :: Word16
}

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
spriteHeight regs =
    case spriteSize regs of
    Size8x8  -> 8
    Size8x16 -> 16
vBlankNMI regs = (control regs .&. 0x80) /= 0
showBackground regs = (mask regs .&. 0x08) /= 0
showSprites regs = (mask regs .&. 0x10) /= 0
setSpriteOverflow val regs = regs { status = setFlag (status regs) 0x20 val }
setSpriteZeroHit val regs = regs { status = setFlag (status regs) 0x40 val }
setInVBlank val regs = regs { status = setFlag (status regs) 0x80 val }
flipHorizontal sprite = (attribute sprite .&. 0x40) /= 0
flip_vertical sprite = (attribute sprite .&. 0x80) /= 0
priority sprite = if attribute sprite .&. 0x20 == 0 then AboveBackground else BelowBackground
onScanline sprite y regs = (y >= spriteY) && (y < (spriteY + spriteHeight regs))
    where spriteY = yPosition sprite
inBoundingBox sprite x y regs =
    (x >= spriteX) && (x < spriteX) && (y >= spriteY) && (y < (spriteY + spriteHeight regs))
    where spriteX = xPosition sprite
          spriteY = yPosition sprite
tiles sprite regs =
    case spriteSize regs of
    Size8x8  -> Tile8x8 $ byteToWord index .|. base
    Size8x16 -> Tile8x16 first $ first + 1
    where base = spritePatternTableAddr regs
          index = tileIndex sprite
          addend = if (index .&. 1) /= 0 then 0x1000 else 0
          first = byteToWord (index .&. 0xf3) + addend

palette :: [RGB]
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
