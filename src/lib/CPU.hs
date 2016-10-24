module CPU where

import Data.Bits ((.|.), (.&.), xor, shiftL, shiftR, complement, testBit)
import Data.Word (Word8, Word16)
import Control.Arrow ((>>>))
import Memory

mapFst f (x, y) = (f x, y)

transfer from to state = to (from state) state

loadByteIncPc :: MachineState -> (Word8, MachineState)
loadByteIncPc state = (transfer pcReg loadByte state, modifyPCReg (+ 1) state)

loadWordIncPc :: MachineState -> (Word16, MachineState)
loadWordIncPc state = (transfer pcReg loadWord state, modifyPCReg (+ 2) state)

pushByte :: Word8 -> MachineState -> MachineState
pushByte val state = modifySReg (subtract 1) $ storeByte (0x0100 + (byteToWord $ sReg state)) val state

-- FIXME: Is this correct? FCEU has two self.storeb()s here. Might have different semantics...
pushWord :: Word16 -> MachineState -> MachineState
pushWord val state = modifySReg (subtract 2) $ storeWord (0x0100 + (byteToWord $ sReg state - 1)) val state

popByte :: MachineState -> (Word8, MachineState)
popByte state = (loadByte (0x0100 + byteToWord (sReg state) + 1) state, modifySReg (+ 1) state)

-- FIXME: see two functions up
popWord :: MachineState -> (Word16, MachineState)
popWord state = (loadWord (0x0100 + byteToWord (sReg state) + 1) state, modifySReg (+ 2) state)

type Loader = MachineState -> Word8
type Storer = Word8 -> MachineState -> MachineState
type Addresser = (Loader, Storer)
type AddresserBuilder = MachineState -> (Addresser, MachineState)
type Instruction = Addresser -> MachineState -> MachineState

implicitMode :: AddresserBuilder
implicitMode state = ((loader, storer), state)
    where loader _ = error "this instruction should not access memory"
          storer _ _ = error "this instruction should not access memory"

accumulatorMode :: AddresserBuilder
accumulatorMode state = ((loader, storer), state)
    where loader = aReg
          storer = setAReg

immediateMode :: AddresserBuilder
immediateMode state = ((loader, storer), newState)
    where (val, newState) = loadByteIncPc state
          loader = const val
          storer _ _ = error "Can't store in immediate mode"

memoryMode :: Word16 -> Addresser
memoryMode addr = (loader, storer)
    where loader = loadByte addr
          storer = storeByte addr

zeroPageMode :: AddresserBuilder
zeroPageMode state = mapFst (memoryMode . byteToWord) (loadByteIncPc state)

zeroPageXMode :: AddresserBuilder
zeroPageXMode state = mapFst (memoryMode . byteToWord . (+ (xReg state))) (loadByteIncPc state)

zeroPageYMode :: AddresserBuilder
zeroPageYMode state = mapFst (memoryMode . byteToWord . (+ (yReg state))) (loadByteIncPc state)

absoluteMode :: AddresserBuilder
absoluteMode state = mapFst memoryMode (loadWordIncPc state)

absoluteXMode :: AddresserBuilder
absoluteXMode state = mapFst (memoryMode . (+ (byteToWord $ xReg state))) (loadWordIncPc state)

absoluteYMode :: AddresserBuilder
absoluteYMode state = mapFst (memoryMode . (+ (byteToWord $ yReg state))) (loadWordIncPc state)

indexedIndirectXMode :: AddresserBuilder
indexedIndirectXMode state = mapFst (memoryMode . flip loadWord state . (+ (byteToWord $ xReg state)) . byteToWord) (loadByteIncPc state)

-- FIXME: this needs some alternate behavior for zero page mem.rs:35
indirectIndexedYMode :: AddresserBuilder
indirectIndexedYMode state = mapFst (memoryMode . (+ (byteToWord $ yReg state)) . flip loadWord state . byteToWord) (loadByteIncPc state)

lda (loader, _) = transfer loader setAReg
ldx (loader, _) = transfer loader setXReg
ldy (loader, _) = transfer loader setYReg
sta (_, storer) = transfer aReg storer
stx (_, storer) = transfer xReg storer
sty (_, storer) = transfer yReg storer

addWithCarry val state = do
    let a = aReg state
    let resultWord = byteToWord a + byteToWord val + (if carryFlag state then 0x0001 else 0x0000)
    let resultByte = wordToByte resultWord
    let carry = resultWord .&. 0x0100 /= 0
    let overflow = (a .&. 0x80 == val .&. 0x80) && (a .&. 0x80 /= resultByte .&. 0x80)
    setAReg resultByte $ setCarryFlag carry $ setOverflowFlag overflow state

adc (loader, _) = transfer loader addWithCarry
sbc (loader, _) = transfer (negate . loader) addWithCarry

comp :: (MachineState -> Word8) -> Instruction
comp reg (loader, _) state = do
    let result = byteToWord (reg state) - byteToWord (loader state)
    setZN (wordToByte result) $ setCarryFlag (testBit result 9) state

cmp = comp aReg
cpx = comp xReg
cpy = comp yReg

add (loader, _) = transfer ((.&.) . loader) modifyAReg
ora (loader, _) = transfer ((.|.) . loader) modifyAReg
eor (loader, _) = transfer (xor . loader) modifyAReg

bit (loader, _) state = do
    let val = loader state
    let overflow = testBit val overflowBit
    let negative = testBit val negativeBit
    let zero = val .&. aReg state == 0
    setOverflowFlag overflow $ setNegativeFlag negative $ setZeroFlag zero state

shiftLeft :: Bool -> Instruction
shiftLeft lsb (loader, storer) state = do
    let val = loader state
    let carry = testBit val 7
    let result = (val `shiftL` 1) .|. (if lsb && carryFlag state then 0x01 else 0x00)
    storer result $ setZN result $ setCarryFlag carry state

shiftRight :: Bool -> Instruction
shiftRight msb (loader, storer) state = do
    let val = loader state
    let carry = testBit val 0
    let result = (val `shiftR` 1) .|. (if msb && carryFlag state then 0x80 else 0x00)
    storer result $ setZN result $ setCarryFlag carry state

rol = shiftLeft  True
ror = shiftRight True
asl = shiftLeft  False
lsr = shiftRight False

inc (loader, storer) state = do
    let val = loader state + 1
    storer val $ setZN val state

dec (loader, storer) state = do
    let val = loader state - 1
    storer val $ setZN val state

inx _ = modifyXReg (+ 1)
dex _ = modifyXReg (subtract 1)
iny _ = modifyYReg (+ 1)
dey _ = modifyYReg (subtract 1)
tax _ = transfer aReg setXReg
tay _ = transfer aReg setYReg
txa _ = transfer xReg setAReg
tya _ = transfer yReg setAReg
txs _ = transfer xReg setSReg
tsx _ = transfer sReg setXReg
clc _ = setCarryFlag    False
sec _ = setCarryFlag    True
cli _ = setIRQFlag      False
sei _ = setIRQFlag      True
clv _ = setOverflowFlag False
cld _ = setDecimalFlag  False -- TODO: decimal flag isn't used since BCD commands are disabled
sed _ = setDecimalFlag  True

branch :: (MachineState -> Bool) -> Instruction
branch condf _ state =
    if condf state
        then uncurry modifyPCReg $ mapFst ((+) . byteToWord) $ loadByteIncPc state
        else state

bpl = branch $ negativeFlag >>> not
bmi = branch $ negativeFlag
bvc = branch $ overflowFlag >>> not
bvs = branch $ overflowFlag
bcc = branch $ carryFlag >>> not
bcs = branch $ carryFlag
bne = branch $ zeroFlag >>> not
beq = branch $ zeroFlag

jmp _ = uncurry setPCReg . loadWordIncPc

-- NOTE: apparently there's a hack here for the hi byte made necessary by bug in 6502 chip ???
jpi _ state = do
    let (addr, state) = loadWordIncPc state
    let lo = byteToWord $ loadByte addr state
    let hi = byteToWord $ loadByte ((addr .&. 0xff00) .|. ((addr + 1) .&. 0x00ff)) state
    setPCReg ((hi `shiftL` 8) .|. lo) state

jsr _ state = do
    let (addr, state) = loadWordIncPc state
    setPCReg addr $ pushWord (pcReg state - 1) state

rts _ = uncurry setPCReg . mapFst (+ 1) . popWord

brk _ state = do
    let pc = pcReg state
    let state = pushWord (pc + 1) state
    -- FIXME: FCEU sets BREAK_FLAG and U_FLAG here, why?
    let state = setIRQFlag True $ transfer flagReg pushByte state
    transfer (loadWord breakVector) setPCReg state

rti _ = uncurry setPCReg . popWord . uncurry setFlagReg . popByte -- NOTE: unlike rts, no +1

pha _ = transfer aReg pushByte
pla _ = uncurry setAReg . popByte
php _ = transfer ((.|. breakMask) . flagReg) pushByte
plp _ = uncurry setFlagReg . popByte
nop _ = id

nmiVector   = 0xfffa :: Word16
resetVector = 0xfffc :: Word16
breakVector = 0xfffe :: Word16

reset = transfer (loadWord resetVector) setPCReg

nmi = transfer (loadWord nmiVector) setPCReg . transfer flagReg pushByte . transfer pcReg pushWord

irqTransfer = transfer (loadWord breakVector) setPCReg . transfer flagReg pushByte . transfer pcReg pushWord

irq state =
    if irqFlag state
        then state
        else irqTransfer state

-- TODO: maybe combine step+eval and have a cycle counter on MachineState, have decode return cycle count

step :: MachineState -> MachineState
step = uncurry eval . loadByteIncPc

eval :: Word8 -> MachineState -> MachineState
eval opCode = uncurry op . builder
    where (op, builder) = decode opCode

decode :: Word8 -> (Instruction, AddresserBuilder)
decode 0xa1 = (lda, indexedIndirectXMode)
decode 0xa5 = (lda, zeroPageMode)
decode 0xa9 = (lda, immediateMode)
decode 0xad = (lda, absoluteMode)
decode 0xb1 = (lda, indirectIndexedYMode)
decode 0xb5 = (lda, zeroPageXMode)
decode 0xb9 = (lda, absoluteYMode)
decode 0xbd = (lda, absoluteXMode)
decode 0xa2 = (ldx, immediateMode)
decode 0xa6 = (ldx, zeroPageMode)
decode 0xb6 = (ldx, zeroPageYMode)
decode 0xae = (ldx, absoluteMode)
decode 0xbe = (ldx, absoluteYMode)
decode 0xa0 = (ldy, immediateMode)
decode 0xa4 = (ldy, zeroPageMode)
decode 0xb4 = (ldy, zeroPageXMode)
decode 0xac = (ldy, absoluteMode)
decode 0xbc = (ldy, absoluteXMode)
decode 0x85 = (sta, zeroPageMode)
decode 0x95 = (sta, zeroPageXMode)
decode 0x8d = (sta, absoluteMode)
decode 0x9d = (sta, absoluteXMode)
decode 0x99 = (sta, absoluteYMode)
decode 0x81 = (sta, indexedIndirectXMode)
decode 0x91 = (sta, indirectIndexedYMode)
decode 0x86 = (stx, zeroPageMode)
decode 0x96 = (stx, zeroPageYMode)
decode 0x8e = (stx, absoluteMode)
decode 0x84 = (sty, zeroPageMode)
decode 0x94 = (sty, zeroPageXMode)
decode 0x8c = (sty, absoluteMode)
decode 0x69 = (adc, immediateMode)
decode 0x65 = (adc, zeroPageMode)
decode 0x75 = (adc, zeroPageXMode)
decode 0x6d = (adc, absoluteMode)
decode 0x7d = (adc, absoluteXMode)
decode 0x79 = (adc, absoluteYMode)
decode 0x61 = (adc, indexedIndirectXMode)
decode 0x71 = (adc, indirectIndexedYMode)
decode 0xe9 = (sbc, immediateMode)
decode 0xe5 = (sbc, zeroPageMode)
decode 0xf5 = (sbc, zeroPageXMode)
decode 0xed = (sbc, absoluteMode)
decode 0xfd = (sbc, absoluteXMode)
decode 0xf9 = (sbc, absoluteYMode)
decode 0xe1 = (sbc, indexedIndirectXMode)
decode 0xf1 = (sbc, indirectIndexedYMode)
decode 0xc9 = (cmp, immediateMode)
decode 0xc5 = (cmp, zeroPageMode)
decode 0xd5 = (cmp, zeroPageXMode)
decode 0xcd = (cmp, absoluteMode)
decode 0xdd = (cmp, absoluteXMode)
decode 0xd9 = (cmp, absoluteYMode)
decode 0xc1 = (cmp, indexedIndirectXMode)
decode 0xd1 = (cmp, indirectIndexedYMode)
decode 0xe0 = (cpx, immediateMode)
decode 0xe4 = (cpx, zeroPageMode)
decode 0xec = (cpx, absoluteMode)
decode 0xc0 = (cpy, immediateMode)
decode 0xc4 = (cpy, zeroPageMode)
decode 0xcc = (cpy, absoluteMode)
decode 0x29 = (add, immediateMode)
decode 0x25 = (add, zeroPageMode)
decode 0x35 = (add, zeroPageXMode)
decode 0x2d = (add, absoluteMode)
decode 0x3d = (add, absoluteXMode)
decode 0x39 = (add, absoluteYMode)
decode 0x21 = (add, indexedIndirectXMode)
decode 0x31 = (add, indirectIndexedYMode)
decode 0x09 = (ora, immediateMode)
decode 0x05 = (ora, zeroPageMode)
decode 0x15 = (ora, zeroPageXMode)
decode 0x0d = (ora, absoluteMode)
decode 0x1d = (ora, absoluteXMode)
decode 0x19 = (ora, absoluteYMode)
decode 0x01 = (ora, indexedIndirectXMode)
decode 0x11 = (ora, indirectIndexedYMode)
decode 0x49 = (eor, immediateMode)
decode 0x45 = (eor, zeroPageMode)
decode 0x55 = (eor, zeroPageXMode)
decode 0x4d = (eor, absoluteMode)
decode 0x5d = (eor, absoluteXMode)
decode 0x59 = (eor, absoluteYMode)
decode 0x41 = (eor, indexedIndirectXMode)
decode 0x51 = (eor, indirectIndexedYMode)
decode 0x24 = (bit, zeroPageMode)
decode 0x2c = (bit, absoluteMode)
decode 0x2a = (rol, accumulatorMode)
decode 0x26 = (rol, zeroPageMode)
decode 0x36 = (rol, zeroPageXMode)
decode 0x2e = (rol, absoluteMode)
decode 0x3e = (rol, absoluteXMode)
decode 0x6a = (ror, accumulatorMode)
decode 0x66 = (ror, zeroPageMode)
decode 0x76 = (ror, zeroPageXMode)
decode 0x6e = (ror, absoluteMode)
decode 0x7e = (ror, absoluteXMode)
decode 0x0a = (asl, accumulatorMode)
decode 0x06 = (asl, zeroPageMode)
decode 0x16 = (asl, zeroPageXMode)
decode 0x0e = (asl, absoluteMode)
decode 0x1e = (asl, absoluteXMode)
decode 0x4a = (lsr, accumulatorMode)
decode 0x46 = (lsr, zeroPageMode)
decode 0x56 = (lsr, zeroPageXMode)
decode 0x4e = (lsr, absoluteMode)
decode 0x5e = (lsr, absoluteXMode)
decode 0xe6 = (inc, zeroPageMode)
decode 0xf6 = (inc, zeroPageXMode)
decode 0xee = (inc, absoluteMode)
decode 0xfe = (inc, absoluteXMode)
decode 0xc6 = (dec, zeroPageMode)
decode 0xd6 = (dec, zeroPageXMode)
decode 0xce = (dec, absoluteMode)
decode 0xde = (dec, absoluteXMode)
decode 0xe8 = (inx, implicitMode)
decode 0xca = (dex, implicitMode)
decode 0xc8 = (iny, implicitMode)
decode 0x88 = (dey, implicitMode)
decode 0xaa = (tax, implicitMode)
decode 0xa8 = (tay, implicitMode)
decode 0x8a = (txa, implicitMode)
decode 0x98 = (tya, implicitMode)
decode 0x9a = (txs, implicitMode)
decode 0xba = (tsx, implicitMode)
decode 0x18 = (clc, implicitMode)
decode 0x38 = (sec, implicitMode)
decode 0x58 = (cli, implicitMode)
decode 0x78 = (sei, implicitMode)
decode 0xb8 = (clv, implicitMode)
decode 0xd8 = (cld, implicitMode)
decode 0xf8 = (sed, implicitMode)
decode 0x10 = (bpl, implicitMode)
decode 0x30 = (bmi, implicitMode)
decode 0x50 = (bvc, implicitMode)
decode 0x70 = (bvs, implicitMode)
decode 0x90 = (bcc, implicitMode)
decode 0xb0 = (bcs, implicitMode)
decode 0xd0 = (bne, implicitMode)
decode 0xf0 = (beq, implicitMode)
decode 0x4c = (jmp, implicitMode)
decode 0x6c = (jpi, implicitMode)
decode 0x20 = (jsr, implicitMode)
decode 0x60 = (rts, implicitMode)
decode 0x00 = (brk, implicitMode)
decode 0x40 = (rti, implicitMode)
decode 0x48 = (pha, implicitMode)
decode 0x68 = (pla, implicitMode)
decode 0x08 = (php, implicitMode)
decode 0x28 = (plp, implicitMode)
decode 0xea = (nop, implicitMode)
decode opCode = error $ "Invalid op code: " ++ show opCode
