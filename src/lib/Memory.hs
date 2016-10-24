{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Memory where

import Data.Bits (Bits, (.|.), (.&.), shiftR, shiftL, complement, bit, testBit)
import Data.Vector.Persistent (Vector, update, index, fromList)
import qualified Data.Vector.Persistent as P
import Data.Word (Word8, Word16)

byteToWord :: Word8 -> Word16
byteToWord = fromIntegral

wordToByte :: Word16 -> Word8
wordToByte = fromIntegral

class Storage m where
    loadByte :: Word16 -> m -> Word8

    loadWord :: Word16 -> m -> Word16
    loadWord addr storage = b0 .|. (b1 `shiftL` 8)
        where b0 = byteToWord $ loadByte addr storage
              b1 = byteToWord $ loadByte (addr + 1) storage

    storeByte :: Word16 -> Word8 -> m -> m

    storeWord :: Word16 -> Word16 -> m -> m
    storeWord addr word = storeByte addr b0 . storeByte (addr + 1) b1
        where b0 = wordToByte $ word .&. 0xff
              b1 = wordToByte $ word `shiftR` 8

type RAM = Vector Word8

malloc :: Int -> RAM
malloc n = fromList (replicate n 0)

instance Storage RAM where
    loadByte addr ram =
        case index ram (fromIntegral addr) of
        Just value -> value
        _ -> error $ "invalid RAM address: " ++ (show addr)
    storeByte addr value ram = update (fromIntegral addr) value ram

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

data MachineState = MachineState {
    ram     :: RAM,
    aReg    :: Word8,
    xReg    :: Word8,
    yReg    :: Word8,
    sReg    :: Word8,
    flagReg :: Word8,
    pcReg   :: Word16,
    control :: Word8,
    mask    :: Word8,
    status  :: Word8,
    oamAddr :: Word8,
    oamData :: RAM
}

defaultState = MachineState {
    ram     = malloc 2048,
    aReg    = 0x00,
    xReg    = 0x00,
    yReg    = 0x00,
    sReg    = 0xfd,
    flagReg = unusedMask .|. irqMask,
    pcReg   = 0xc000,
    control = 0x00,
    mask    = 0x00,
    status  = 0x00,
    oamAddr = 0x00,
    oamData = malloc 256
}

setAReg, setXReg, setYReg, setSReg, setFlagReg, setZN :: Word8 -> MachineState -> MachineState
setAReg value state = (setZN value state) { aReg = value }
setXReg value state = (setZN value state) { xReg = value }
setYReg value state = (setZN value state) { yReg = value }
setSReg value state = state { sReg = value }
setFlagReg value state = state { flagReg = (value .|. unusedMask) .&. (complement breakMask) }
setZN value = (setZeroFlag $ value == 0) . (setNegativeFlag $ testBit value 7)
setControlReg value state = state { control = value }
setMaskReg value state = state { mask = value }
setStatusReg value state = state { status = value }
setOAMAddr value state = state { oamAddr = value }

setPCReg :: Word16 -> MachineState -> MachineState
setPCReg value state = state { pcReg = value }

setFlag :: Word8 -> Bool -> MachineState -> MachineState
setFlag mask value state = 
    let flags = flagReg state in
    state {
        flagReg =
            if value
                then flags .|. mask
                else flags .&. (complement mask)
    }

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

modifyAReg, modifyXReg, modifyYReg, modifySReg :: (Word8 -> Word8) -> MachineState -> MachineState
modifyAReg f state = setAReg (f $ aReg state) state
modifyXReg f state = setXReg (f $ xReg state) state
modifyYReg f state = setYReg (f $ yReg state) state
modifySReg f state = setSReg (f $ sReg state) state

modifyPCReg :: (Word16 -> Word16) -> MachineState -> MachineState
modifyPCReg f state = setPCReg (f $ pcReg state) state

screenWidth = 256
screenHeight = 240
cyclesPerScanline = 114
vBlankScanline = 241
lastScanline = 261

xScrollOffset, yScrollOffset, vramAddrIncrement, spritePatternTableAddr, backgroundPatternTableAddr :: MachineState -> Word16
xScrollOffset state              = if testBit (control state) 0 then screenWidth else 0x0000
yScrollOffset state              = if testBit (control state) 1 then screenHeight else 0x0000
vramAddrIncrement state          = if testBit (control state) 2 then 0x0020 else 0x0001
spritePatternTableAddr state     = if testBit (control state) 4 then 0x1000 else 0x0000
backgroundPatternTableAddr state = if testBit (control state) 5 then 0x1000 else 0x0000

spriteSize :: MachineState -> SpriteSize
spriteSize state = if testBit (control state) 6 then Size8x16 else Size8x8

spriteHeight :: MachineState -> Word8
spriteHeight state =
    case spriteSize state of
    Size8x8  -> 8
    Size8x16 -> 16

vBlankNMI :: MachineState -> Bool
vBlankNMI state = testBit (control state) 7

{-
    scroll: PpuScroll,  // PPUSCROLL: 0x2005
    addr: PpuAddr,      // PPUADDR: 0x2006
-}

dmaTransfer :: Word8 -> MachineState -> MachineState
dmaTransfer value state = do
    let start = byteToWord value `shiftL` 8
    state { oamData = P.fromList $ map (flip loadByte state) $ map (+ start) [0..255] }

instance Storage MachineState where
    loadByte addr state =
        if addr < 0x2000 then loadByte addr (ram state) else
        if addr == 0x2000 then control state else
        if addr == 0x2001 then mask state else
        if addr == 0x2002 then status state else
        if addr == 0x2003 then error "attempt to read from OAM Addr" else -- oamAddr state else
        if addr == 0x2004 then error "attempt to read from OAM Data" else
        if addr == 0x4014 then error "attempt to read from DMA Trigger 0x4014" else
        error $ "Storage MachineState loadByte: Address out of range: " ++ addr

    storeByte addr value state =
        if addr < 0x2000 then state { ram = storeByte addr value (ram state) } else
        if addr == 0x2000 then setControlReg value state else
        if addr == 0x2001 then setMaskReg value state else
        if addr == 0x2002 then setStatusReg value state else
        if addr == 0x2003 then error "attempt to write to OAM Addr" else -- setOAMAddr value state else
        if addr == 0x2004 then error "attempt to write to OAM Data" else
        if addr == 0x4014 then dmaTransfer value state else
        error $ "Storage MachineState storeByte: Address out of range: " ++ addr
