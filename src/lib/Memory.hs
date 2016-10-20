{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Memory where

import Data.Bits ((.|.), (.&.), shiftR, shiftL, complement)
import Data.Vector.Persistent (Vector, update, index, fromList)
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

data MachineState = MachineState {
    ram     :: RAM,
    aReg    :: Word8,
    xReg    :: Word8,
    yReg    :: Word8,
    sReg    :: Word8,
    flagReg :: Word8,
    pcReg   :: Word16
}

defaultState = MachineState {
    ram     = malloc 2048,
    aReg    = 0x00,
    xReg    = 0x00,
    yReg    = 0x00,
    sReg    = 0xfd,
    flagReg = 0x24,
    pcReg   = 0xc000
}

setAReg :: Word8 -> MachineState -> MachineState
setAReg value state = (setZN value state) { aReg = value }

setXReg :: Word8 -> MachineState -> MachineState
setXReg value state = (setZN value state) { xReg = value }

setYReg :: Word8 -> MachineState -> MachineState
setYReg value state = (setZN value state) { yReg = value }

setSReg :: Word8 -> MachineState -> MachineState
setSReg value state = state { sReg = value }

setPCReg :: Word16 -> MachineState -> MachineState
setPCReg value state = state { pcReg = value }

setFlagReg :: Word8 -> MachineState -> MachineState
setFlagReg value state = state { flagReg = (value .|. 0x30) - 0x10 }

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

carryMask    = 0x01 :: Word8
zeroMask     = 0x02 :: Word8
irqMask      = 0x04 :: Word8
decimalMask  = 0x08 :: Word8
breakMask    = 0x10 :: Word8
overflowMask = 0x40 :: Word8
negativeMask = 0x80 :: Word8

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

setZN :: Word8 -> MachineState -> MachineState
setZN value = (setZeroFlag $ value == 0) . (setNegativeFlag $ value .&. 0x80 /= 0)

modifyAReg :: (Word8 -> Word8) -> MachineState -> MachineState
modifyAReg f state = setAReg (f $ aReg state) state

modifyXReg :: (Word8 -> Word8) -> MachineState -> MachineState
modifyXReg f state = setXReg (f $ xReg state) state

modifyYReg :: (Word8 -> Word8) -> MachineState -> MachineState
modifyYReg f state = setYReg (f $ yReg state) state

modifySReg :: (Word8 -> Word8) -> MachineState -> MachineState
modifySReg f state = setSReg (f $ sReg state) state

modifyPCReg :: (Word16 -> Word16) -> MachineState -> MachineState
modifyPCReg f state = setPCReg (f $ pcReg state) state

instance Storage MachineState where
    loadByte addr state =
        if addr < 0x2000 then loadByte addr (ram state) else
        error "out of range"

    storeByte addr val state =
        if addr < 0x2000 then state { ram = storeByte addr val (ram state) } else
        if addr == 0x4014 then do -- Writing to 0x4014 triggers DMA transfer
            let start = (byteToWord val) `shiftL` 8
            let move ad st = storeByte 0x2004 (loadByte ad st) st
            foldr move state [start .. (start + 255)]
        else
        error "out of range"
