{-# LANGUAGE FlexibleInstances #-}

module Memory where

import Data.Bits (Bits, (.|.), (.&.), shiftR, shiftL, complement, bit, testBit, setBit)
import Data.Default (Default(..))
import Data.Vector.Persistent (Vector, update, index, fromList)
import qualified Data.Vector.Persistent as P
import Data.Word (Word8, Word16)

transfer :: (a -> b) -> (b -> a -> c) -> a -> c
transfer from to state = to (from state) state

byteToWord :: Word8 -> Word16
byteToWord = fromIntegral

wordToByte :: Word16 -> Word8
wordToByte = fromIntegral

class Storage m where
    loadByte :: Word16 -> m -> Word8

    loadWord :: Word16 -> m -> Word16
    loadWord addr storage = b0 .|. b1 `shiftL` 8
        where b0 = byteToWord $ loadByte addr storage
              b1 = byteToWord $ loadByte (addr + 1) storage

    storeByte :: Word16 -> Word8 -> m -> m

    storeWord :: Word16 -> Word16 -> m -> m
    storeWord addr word = storeByte addr b0 . storeByte (addr + 1) b1
        where b0 = wordToByte $ word .&. 0xff
              b1 = wordToByte $ word `shiftR` 8

vector :: Default d => Int -> Vector d
vector n = fromList (replicate n def)

instance Storage (Vector Word8) where
    loadByte addr ram =
        case index ram (fromIntegral addr) of
        Just value -> value
        _ -> error $ "invalid RAM address: " ++ show addr
    storeByte = update . fromIntegral

data Sprite = Sprite {
    xPosition :: Word8,
    yPosition :: Word8,
    tileIndex :: Word8,
    attribute :: Word8
}

data SpriteSize = Size8x8 | Size8x16
data SpritePriority = AboveBackground | BelowBackground
data ScrollDirection = XDirection | YDirection
data PixelLayer = BackgroundLayer | SpriteLayer

data NametableAddress = NametableAddress {
    xIndex :: Word8,
    yIndex :: Word8,
    base :: Word16
}

type Color = (Word8, Word8, Word8)
type SpriteColor = (SpritePriority, Color)
data SpriteTile = Tile8x8 Word16 | Tile8x16 Word16 Word16
type Screen = Vector Color

data MachineState = MachineState {
    cycleCount   :: Int,
    ram          :: Vector Word8,
    aReg         :: Word8,
    xReg         :: Word8,
    yReg         :: Word8,
    sReg         :: Word8,
    flagReg      :: Word8,
    pcReg        :: Word16,
    controlReg   :: Word8,
    maskReg      :: Word8,
    statusReg    :: Word8,
    oamAddr      :: Word8,
    oamData      :: Vector Word8,
    ppuAddr      :: Word16,
    ppuAddrHi    :: Bool,
    ppuScrollX   :: Word8,
    ppuScrollY   :: Word8,
    ppuScrollDir :: ScrollDirection,
    nametables   :: Vector Word8,
    palette      :: Vector Word8,
    screen       :: Vector Color
}

defaultState = MachineState {
    cycleCount   = 0,
    ram          = vector 2048,
    aReg         = 0x00,
    xReg         = 0x00,
    yReg         = 0x00,
    sReg         = 0xfd,
    flagReg      = unusedMask .|. irqMask,
    pcReg        = 0xc000,
    controlReg   = 0x00,
    maskReg      = 0x00,
    statusReg    = 0x00,
    oamAddr      = 0x00,
    oamData      = vector 256,
    ppuAddr      = 0x0000,
    ppuAddrHi    = True,
    ppuScrollX   = 0x00,
    ppuScrollY   = 0x00,
    ppuScrollDir = XDirection,
    nametables   = vector 2048,
    palette      = vector 32,
    screen       = vector (256 * 240)
}

addCycles cycles state = state { cycleCount = (cycleCount state) + cycles }

setRAM        value state = state { ram = value }
setAReg       value state = (setZN value state) { aReg = value }
setXReg       value state = (setZN value state) { xReg = value }
setYReg       value state = (setZN value state) { yReg = value }
setSReg       value state = state { sReg = value }
setPCReg      value state = state { pcReg = value }
setFlagReg    value state = state { flagReg = (value .|. unusedMask) .&. complement breakMask }
setControlReg value state = state { controlReg = value }
setMaskReg    value state = state { maskReg = value }
setStatusReg  value state = state { statusReg = value }

setZN :: Word8 -> MachineState -> MachineState
setZN value = setZeroFlag (value == 0) . setNegativeFlag (testBit value 7)

setFlag :: Word8 -> Bool -> MachineState -> MachineState
setFlag mask value state = state { flagReg = op mask (flagReg state) }
    where op = if value then (.|.) else (.&.) . complement

getFlag :: Word8 -> MachineState -> Bool
getFlag mask state = flagReg state .&. mask /= 0

carryBit, zeroBit, irqBit, decimalBit, breakBit, unusedBit, overflowBit, negativeBit :: Int
carryBit    = 0
zeroBit     = 1
irqBit      = 2
decimalBit  = 3
breakBit    = 4
unusedBit   = 5
overflowBit = 6
negativeBit = 7

carryMask, zeroMask, irqMask, decimalMask, breakMask, unusedMask, overflowMask, negativeMask :: Word8
carryMask    = bit carryBit
zeroMask     = bit zeroBit
irqMask      = bit irqBit
decimalMask  = bit decimalBit -- TODO: unused since BCD is disabled in NES
breakMask    = bit breakBit
unusedMask   = bit unusedBit -- NOTE: should always be 1 for some reason
overflowMask = bit overflowBit
negativeMask = bit negativeBit

setCarryFlag    = setFlag carryMask
setZeroFlag     = setFlag zeroMask
setIRQFlag      = setFlag irqMask
setDecimalFlag  = setFlag decimalMask
setBreakFlag    = setFlag breakMask
setOverflowFlag = setFlag overflowMask
setNegativeFlag = setFlag negativeMask

carryFlag    = getFlag carryMask
zeroFlag     = getFlag zeroMask
irqFlag      = getFlag irqMask
decimalFlag  = getFlag decimalMask
breakFlag    = getFlag breakMask
overflowFlag = getFlag overflowMask
negativeFlag = getFlag negativeMask

modifyRAM       f = transfer (f . ram) setRAM
modifyAReg      f = transfer (f . aReg) setAReg
modifyXReg      f = transfer (f . xReg) setXReg
modifyYReg      f = transfer (f . yReg) setYReg
modifySReg      f = transfer (f . sReg) setSReg
modifyPCReg     f = transfer (f . pcReg) setPCReg
modifyStatusReg f = transfer (f . statusReg) setStatusReg

xScrollOffset              state =    testBit (controlReg state) 0
yScrollOffset              state =    testBit (controlReg state) 1
vramAddrIncrement          state = if testBit (controlReg state) 2 then 0x0020 else 0x0001 :: Word16
spritePatternTableAddr     state = if testBit (controlReg state) 3 then 0x1000 else 0x0000 :: Word16
backgroundPatternTableAddr state = if testBit (controlReg state) 4 then 0x1000 else 0x0000 :: Word16
spriteSize                 state = if testBit (controlReg state) 5 then Size8x16 else Size8x8
ppuMasterSlave             state =    testBit (controlReg state) 6
vBlankNMI                  state =    testBit (controlReg state) 7

isGrayscale        state = testBit (maskReg state) 0
showBackgroundLeft state = testBit (maskReg state) 1
showSpritesLeft    state = testBit (maskReg state) 2
showBackground     state = testBit (maskReg state) 3
showSprites        state = testBit (maskReg state) 4
enhancedReds       state = testBit (maskReg state) 5
enhancedGreens     state = testBit (maskReg state) 6
enhancedBlues      state = testBit (maskReg state) 7

setSpriteOverflow value = if value then modifyStatusReg (.|. bit 5) else id
setSpriteZeroHit  value = if value then modifyStatusReg (.|. bit 6) else id
setInVBlank       value = if value then modifyStatusReg (.|. bit 7) else id

incOAMAddr :: MachineState -> MachineState
incOAMAddr state = state { oamAddr = oamAddr state + 1 }

loadOAMByte :: MachineState -> Word8
loadOAMByte state = case index (oamData state) (fromIntegral $ oamAddr state) of
    Just value -> value
    _ -> error $ "Invalid OAM address: " ++ show (oamAddr state)

storeOAMByte :: Word8 -> MachineState -> MachineState
storeOAMByte value state = (incOAMAddr state) { oamData = update (fromIntegral $ oamAddr state) value (oamData state) }

storePPUAddrByte :: Word8 -> MachineState -> MachineState
storePPUAddrByte value state = state { ppuAddr = newAddr, ppuAddrHi = not hi }
    where hi = ppuAddrHi state
          addr = ppuAddr state
          value16 = byteToWord value
          newAddr = if hi then addr .&. 0x00ff .|. value16 `shiftL` 8
                          else addr .&. 0xff00 .|. value16

storePPUScrollByte :: Word8 -> MachineState -> MachineState
storePPUScrollByte value state = case ppuScrollDir state of
    XDirection -> state { ppuScrollX = value, ppuScrollDir = YDirection }
    YDirection -> state { ppuScrollY = value, ppuScrollDir = XDirection }

dmaTransfer :: Word8 -> MachineState -> MachineState
dmaTransfer value state = do
    let start = byteToWord value `shiftL` 8
    let lookup = flip loadByte state . (+ start)
    state { oamData = P.fromList $ map lookup [0..255] }

instance Storage MachineState where
    loadByte addr state
        | addr <  0x2000 = loadByte (addr .&. 0x07ff) (ram state)
        | addr == 0x2000 = controlReg state
        | addr == 0x2001 = maskReg state
        | addr == 0x2002 = statusReg state
        | addr == 0x2003 = error "attempt to read from OAM Addr 0x2003"
        | addr == 0x2004 = loadOAMByte state
        | addr == 0x2005 = error "attempt to read from PPU Scroll 0x2005"
        | addr == 0x2006 = error "attempt to read from PPU Addr 0x2006"
        | addr == 0x2007 = error "attempt to read from PPU Data 0x2007"
        | addr == 0x4014 = error "attempt to read from DMA Trigger 0x4014"
        | otherwise = error $ "Storage MachineState loadByte: Address out of range: " ++ show addr

    storeByte addr value state
        | addr <  0x2000 = modifyRAM (storeByte (addr .&. 0x07ff) value) state
        | addr == 0x2000 = setControlReg value state
        | addr == 0x2001 = setMaskReg value state
        | addr == 0x2002 = setStatusReg value state
        | addr == 0x2003 = state { oamAddr = value }
        | addr == 0x2004 = storeOAMByte value state
        | addr == 0x2005 = storePPUScrollByte value state
        | addr == 0x2006 = storePPUAddrByte value state
        | addr == 0x2007 = error "attempt to write to PPU Data 0x2007"
        | addr == 0x4014 = dmaTransfer value state
        | otherwise = error $ "Storage MachineState storeByte: Address out of range: " ++ show addr
