module CPU where

import Data.Bits ((.|.), (.&.), shiftL, complement)
import Data.Word (Word8, Word16)
import Memory

data Registers = Registers {
    accReg  :: Word8,
    xReg    :: Word8,
    yReg    :: Word8,
    sReg    :: Word8,
    flagReg :: Word8,
    pcReg   :: Word16
}

carryMask    = 0x01 :: Word8
zeroMask     = 0x02 :: Word8
irqMask      = 0x04 :: Word8
decimalMask  = 0x08 :: Word8
breakMask    = 0x10 :: Word8
overflowMask = 0x40 :: Word8
negativeMask = 0x80 :: Word8

nmiVector   = 0xfffa :: Word16
resetVector = 0xfffc :: Word16
breakVector = 0xfffe :: Word16

getFlag :: Word8 -> Registers -> Bool
getFlag mask regs = (flagReg regs .&. mask) == 0

setFlag :: Word8 -> Bool -> Registers -> Registers
setFlag mask val regs =
    let flags = flagReg regs in
    regs {
        flagReg =
            if val
                then flags .|. mask
                else flags .&. (complement mask)
    }

setFlags :: Word8 -> Registers -> Registers
setFlags val regs = regs { flagReg = (val .|. 0x30) - 0x10 }

setZN :: Word8 -> Registers -> Registers
setZN val = (setFlag zeroMask $ val == 0) . (setFlag negativeMask $ (val .&. 0x80) /= 0)

loadIncPc :: Ram -> Registers -> IO (Ram, Registers, Word8)
loadIncPc ram regs = do
    let pc = pcReg regs
    val <- load ram pc
    return (ram, regs { pcReg = pc + 1 }, val)

loadWordIncPc :: Ram -> Registers -> IO (Ram, Registers, Word16)
loadWordIncPc ram regs = do
    let pc = pcReg regs
    val <- loadWord ram pc
    return (ram, regs { pcReg = pc + 2 }, val)

branch :: Ram -> Registers -> Bool -> IO (Ram, Registers)
branch ram regs cond =
    if cond
        then return (ram, regs)
        else do
            (ram, regs, disp) <- loadIncPc ram regs
            let pc = pcReg regs
            return (ram, regs { pcReg = pc + fromIntegral disp })

push :: Ram -> Registers -> Word8 -> IO (Ram, Registers)
push ram regs val = do
    let s = sReg regs
    store ram (0x0100 + fromIntegral s) val
    return (ram, regs { sReg = s - 1 })

-- FIXME: Is this correct? FCEU has two self.storeb()s here. Might have different semantics...
pushWord :: Ram -> Registers -> Word16 -> IO (Ram, Registers)
pushWord ram regs val = do
    let s = sReg regs
    storeWord ram (0x0100 + fromIntegral s - 1) val
    return (ram, regs { sReg = s - 2 })

pop :: Ram -> Registers -> IO (Ram, Registers, Word8)
pop ram regs = do
    let s = sReg regs
    val <- load ram (0x0100 + fromIntegral s + 1)
    return (ram, regs { sReg = s + 1 }, val)

-- FIXME: see two functions up
popWord :: Ram -> Registers -> IO (Ram, Registers, Word16)
popWord ram regs = do
    let s = sReg regs
    val <- loadWord ram (0x0100 + fromIntegral s + 1)
    return (ram, regs { sReg = s + 2 }, val)

data AddressMode = AccumulatorMode | ImmediateMode | MemoryMode

lda AccumulatorMode regs ram = return ()
lda ImmediateMode regs ram = return ()
lda MemoryMode regs ram = return ()

inc AccumulatorMode ram regs = let val = (accReg regs) + 1 in return (ram, (setZN val regs) { accReg = val })
inc _ ram regs = error "other address modes not defined"

dec AccumulatorMode ram regs = let val = (accReg regs) + 1 in return (ram, (setZN val regs) { accReg = val })
dec _ ram regs = error "other address modes not defined"

inx regs = let x = xReg regs + 1 in (setZN x regs) { xReg = x }
dex regs = let x = xReg regs - 1 in (setZN x regs) { xReg = x }
iny regs = let y = yReg regs + 1 in (setZN y regs) { yReg = y }
dey regs = let y = yReg regs - 1 in (setZN y regs) { yReg = y }
tax regs = let acc = accReg regs in (setZN acc regs) { xReg = acc }
tay regs = let acc = accReg regs in (setZN acc regs) { yReg = acc }
txa regs = let x = xReg regs in (setZN x regs) { accReg = x }
tya regs = let y = yReg regs in (setZN y regs) { accReg = y }
txs regs = let x = xReg regs in regs { sReg = x }
tsx regs = let s = sReg regs in (setZN s regs) { xReg = s }
clc = setFlag carryMask False
sec = setFlag carryMask True
cli = setFlag irqMask False
sei = setFlag irqMask True
clv = setFlag overflowMask False
cld = setFlag decimalMask False
sed = setFlag decimalMask True
bpl ram regs = branch ram regs $ not $ getFlag negativeMask regs
bmi ram regs = branch ram regs $ getFlag negativeMask regs
bvc ram regs = branch ram regs $ not $ getFlag overflowMask regs
bvs ram regs = branch ram regs $ getFlag overflowMask regs
bcc ram regs = branch ram regs $ not $ getFlag carryMask regs
bcs ram regs = branch ram regs $ getFlag carryMask regs
bne ram regs = branch ram regs $ not $ getFlag zeroMask regs
beq ram regs = branch ram regs $ getFlag zeroMask regs
jmp ram regs = do
    (ram, regs, addr) <- loadWordIncPc ram regs
    return (ram, regs { pcReg = addr })
jmpi ram regs = do
    (ram, regs, addr) <- loadWordIncPc ram regs

    -- NOTE: apparently made necessary by bug in 6502 chip ???
    lo <- load ram addr
    hi <- load ram ((addr .&. 0xff00) .|. ((addr + 1) .&. 0x00ff))
    let addr = fromIntegral $ (hi `shiftL` 8) .|. lo

    return (ram, regs { pcReg = addr })

jsr ram regs = do
    (ram, regs, addr) <- loadWordIncPc ram regs
    let pc = pcReg regs
    (ram, regs) <- pushWord ram regs (pc - 1)
    return (ram, regs { pcReg = addr })

rts ram regs = do
    (ram, regs, addr) <- popWord ram regs
    return (ram, regs { pcReg = addr + 1 })

brk ram regs = do
    let pc = pcReg regs
    (ram, regs) <- pushWord ram regs (pc + 1)
    let flags = flagReg regs
    (ram, regs) <- push ram regs flags -- FIXME: FCEU sets BREAK_FLAG and U_FLAG here, why?
    let regs = setFlag irqMask True regs
    addr <- loadWord ram breakVector
    return (ram, regs { pcReg = addr })

rit ram regs = do
    (ram, regs, flags) <- pop ram regs
    let regs = setFlags flags regs
    (ram, regs, addr) <- popWord ram regs
    return (ram, regs { pcReg = addr }) -- NOTE: no +1

pha ram regs = push ram regs (accReg regs)

pla ram regs = do
    (ram, regs, val) <- pop ram regs
    return (ram, (setZN val regs) { accReg = val })

php ram regs = push ram regs (flagReg regs .|. breakMask)

plp ram regs = do
    (ram, regs, flags) <- pop ram regs
    return (ram, setFlags flags regs)

nop = id

{-The main fetch-and-decode routine
    pub fn step(&mut self) {
        self.trace();

        let op = self.loadb_bump_pc();
        decode_op!(op, self);

        self.cy += CYCLE_TABLE[op as usize] as Cycles;
    }
-}

reset ram regs = do
    addr <- loadWord ram resetVector
    return (ram, regs { pcReg = addr })

nmi ram regs = do
    let pc = pcReg regs
    let flags = flagReg regs
    (ram, regs) <- pushWord ram regs pc
    (ram, regs) <- push ram regs flags
    addr <- loadWord ram nmiVector
    return (ram, regs { pcReg = addr })

irq ram regs =
    if getFlag irqMask regs
        then return (ram, regs)
        else do
            let pc = pcReg regs
            let flags = flagReg regs
            (ram, regs) <- pushWord ram regs pc
            (ram, regs) <- push ram regs flags
            addr <- loadWord ram breakVector
            return (ram, regs { pcReg = addr })
