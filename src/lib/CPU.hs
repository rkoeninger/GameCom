module CPU where

import Data.Bits ((.|.), (.&.), xor, shiftL, shiftR, complement, testBit)
import Data.Word (Word8, Word16)
import Control.Arrow ((>>>))
import Memory

mapFst f (x, y) = (f x, y)

loadByteIncPc :: MachineState -> (Word8, MachineState)
loadByteIncPc state = (transfer pcReg loadByte state, modifyPCReg (+ 1) state)

loadWordIncPc :: MachineState -> (Word16, MachineState)
loadWordIncPc state = (transfer pcReg loadWord state, modifyPCReg (+ 2) state)

pushByte :: Word8 -> MachineState -> MachineState
pushByte val state = modifySReg (subtract 1) $ storeByte (0x0100 + byteToWord (sReg state)) val state

-- FIXME: Is this correct? FCEU has two self.storeb()s here. Might have different semantics...
pushWord :: Word16 -> MachineState -> MachineState
pushWord val state = modifySReg (subtract 2) $ storeWord (0x0100 + byteToWord (sReg state - 1)) val state

popByte :: MachineState -> (Word8, MachineState)
popByte state = (loadByte (0x0100 + byteToWord (sReg state) + 1) state, modifySReg (+ 1) state)

-- FIXME: see two functions up
popWord :: MachineState -> (Word16, MachineState)
popWord state = (loadWord (0x0100 + byteToWord (sReg state) + 1) state, modifySReg (+ 2) state)

type Loader = MachineState -> Word8
type Storer = Word8 -> MachineState -> MachineState
type Addresser = (Loader, Storer)
type AddresserBuilder = MachineState -> (Addresser, MachineState)
type Operation = Addresser -> MachineState -> MachineState

impMode :: AddresserBuilder
impMode state = ((loader, storer), state)
    where loader _ = error "This operation should not access memory"
          storer _ _ = error "This operation should not access memory"

accMode :: AddresserBuilder
accMode state = ((loader, storer), state)
    where loader = aReg
          storer = setAReg

imdMode :: AddresserBuilder
imdMode state = ((loader, storer), newState)
    where (val, newState) = loadByteIncPc state
          loader = const val
          storer _ _ = error "Can't store in immediate mode"

memoryMode :: Word16 -> Addresser
memoryMode addr = (loader, storer)
    where loader = loadByte addr
          storer = storeByte addr

zpgMode :: AddresserBuilder
zpgMode state = mapFst (memoryMode . byteToWord) (loadByteIncPc state)

zpxMode :: AddresserBuilder
zpxMode state = mapFst (memoryMode . byteToWord . (+ xReg state)) (loadByteIncPc state)

zpyMode :: AddresserBuilder
zpyMode state = mapFst (memoryMode . byteToWord . (+ yReg state)) (loadByteIncPc state)

absMode :: AddresserBuilder
absMode state = mapFst memoryMode (loadWordIncPc state)

abxMode :: AddresserBuilder
abxMode state = mapFst (memoryMode . (+ byteToWord (xReg state))) (loadWordIncPc state)

abyMode :: AddresserBuilder
abyMode state = mapFst (memoryMode . (+ byteToWord (yReg state))) (loadWordIncPc state)

iixMode :: AddresserBuilder
iixMode state = mapFst (memoryMode . flip loadWord state . (+ (byteToWord $ xReg state)) . byteToWord) (loadByteIncPc state)

-- FIXME: this needs some alternate behavior for zero page mem.rs:35
iiyMode :: AddresserBuilder
iiyMode state = mapFst (memoryMode . (+ (byteToWord $ yReg state)) . flip loadWord state . byteToWord) (loadByteIncPc state)

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

comp :: (MachineState -> Word8) -> Operation
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

shiftLeft :: Bool -> Operation
shiftLeft lsb (loader, storer) state = do
    let val = loader state
    let carry = testBit val 7
    let result = val `shiftL` 1 .|. (if lsb && carryFlag state then 0x01 else 0x00)
    storer result $ setZN result $ setCarryFlag carry state

shiftRight :: Bool -> Operation
shiftRight msb (loader, storer) state = do
    let val = loader state
    let carry = testBit val 0
    let result = val `shiftR` 1 .|. (if msb && carryFlag state then 0x80 else 0x00)
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

branch :: (MachineState -> Bool) -> Operation
branch condf _ state =
    if condf state
        then uncurry modifyPCReg $ mapFst ((+) . byteToWord) $ loadByteIncPc state
        else state

bpl = branch $ negativeFlag >>> not
bvc = branch $ overflowFlag >>> not
bcc = branch $ carryFlag >>> not
bne = branch $ zeroFlag >>> not
bvs = branch overflowFlag
bmi = branch negativeFlag
bcs = branch carryFlag
beq = branch zeroFlag

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

decode :: Word8 -> (Operation, AddresserBuilder)
decode 0xa1 = (lda, iixMode)
decode 0xa5 = (lda, zpgMode)
decode 0xa9 = (lda, imdMode)
decode 0xad = (lda, absMode)
decode 0xb1 = (lda, iiyMode)
decode 0xb5 = (lda, zpxMode)
decode 0xb9 = (lda, abyMode)
decode 0xbd = (lda, abxMode)
decode 0xa2 = (ldx, imdMode)
decode 0xa6 = (ldx, zpgMode)
decode 0xb6 = (ldx, zpyMode)
decode 0xae = (ldx, absMode)
decode 0xbe = (ldx, abyMode)
decode 0xa0 = (ldy, imdMode)
decode 0xa4 = (ldy, zpgMode)
decode 0xb4 = (ldy, zpxMode)
decode 0xac = (ldy, absMode)
decode 0xbc = (ldy, abxMode)
decode 0x85 = (sta, zpgMode)
decode 0x95 = (sta, zpxMode)
decode 0x8d = (sta, absMode)
decode 0x9d = (sta, abxMode)
decode 0x99 = (sta, abyMode)
decode 0x81 = (sta, iixMode)
decode 0x91 = (sta, iiyMode)
decode 0x86 = (stx, zpgMode)
decode 0x96 = (stx, zpyMode)
decode 0x8e = (stx, absMode)
decode 0x84 = (sty, zpgMode)
decode 0x94 = (sty, zpxMode)
decode 0x8c = (sty, absMode)
decode 0x69 = (adc, imdMode)
decode 0x65 = (adc, zpgMode)
decode 0x75 = (adc, zpxMode)
decode 0x6d = (adc, absMode)
decode 0x7d = (adc, abxMode)
decode 0x79 = (adc, abyMode)
decode 0x61 = (adc, iixMode)
decode 0x71 = (adc, iiyMode)
decode 0xe9 = (sbc, imdMode)
decode 0xe5 = (sbc, zpgMode)
decode 0xf5 = (sbc, zpxMode)
decode 0xed = (sbc, absMode)
decode 0xfd = (sbc, abxMode)
decode 0xf9 = (sbc, abyMode)
decode 0xe1 = (sbc, iixMode)
decode 0xf1 = (sbc, iiyMode)
decode 0xc9 = (cmp, imdMode)
decode 0xc5 = (cmp, zpgMode)
decode 0xd5 = (cmp, zpxMode)
decode 0xcd = (cmp, absMode)
decode 0xdd = (cmp, abxMode)
decode 0xd9 = (cmp, abyMode)
decode 0xc1 = (cmp, iixMode)
decode 0xd1 = (cmp, iiyMode)
decode 0xe0 = (cpx, imdMode)
decode 0xe4 = (cpx, zpgMode)
decode 0xec = (cpx, absMode)
decode 0xc0 = (cpy, imdMode)
decode 0xc4 = (cpy, zpgMode)
decode 0xcc = (cpy, absMode)
decode 0x29 = (add, imdMode)
decode 0x25 = (add, zpgMode)
decode 0x35 = (add, zpxMode)
decode 0x2d = (add, absMode)
decode 0x3d = (add, abxMode)
decode 0x39 = (add, abyMode)
decode 0x21 = (add, iixMode)
decode 0x31 = (add, iiyMode)
decode 0x09 = (ora, imdMode)
decode 0x05 = (ora, zpgMode)
decode 0x15 = (ora, zpxMode)
decode 0x0d = (ora, absMode)
decode 0x1d = (ora, abxMode)
decode 0x19 = (ora, abyMode)
decode 0x01 = (ora, iixMode)
decode 0x11 = (ora, iiyMode)
decode 0x49 = (eor, imdMode)
decode 0x45 = (eor, zpgMode)
decode 0x55 = (eor, zpxMode)
decode 0x4d = (eor, absMode)
decode 0x5d = (eor, abxMode)
decode 0x59 = (eor, abyMode)
decode 0x41 = (eor, iixMode)
decode 0x51 = (eor, iiyMode)
decode 0x24 = (bit, zpgMode)
decode 0x2c = (bit, absMode)
decode 0x2a = (rol, accMode)
decode 0x26 = (rol, zpgMode)
decode 0x36 = (rol, zpxMode)
decode 0x2e = (rol, absMode)
decode 0x3e = (rol, abxMode)
decode 0x6a = (ror, accMode)
decode 0x66 = (ror, zpgMode)
decode 0x76 = (ror, zpxMode)
decode 0x6e = (ror, absMode)
decode 0x7e = (ror, abxMode)
decode 0x0a = (asl, accMode)
decode 0x06 = (asl, zpgMode)
decode 0x16 = (asl, zpxMode)
decode 0x0e = (asl, absMode)
decode 0x1e = (asl, abxMode)
decode 0x4a = (lsr, accMode)
decode 0x46 = (lsr, zpgMode)
decode 0x56 = (lsr, zpxMode)
decode 0x4e = (lsr, absMode)
decode 0x5e = (lsr, abxMode)
decode 0xe6 = (inc, zpgMode)
decode 0xf6 = (inc, zpxMode)
decode 0xee = (inc, absMode)
decode 0xfe = (inc, abxMode)
decode 0xc6 = (dec, zpgMode)
decode 0xd6 = (dec, zpxMode)
decode 0xce = (dec, absMode)
decode 0xde = (dec, abxMode)
decode 0xe8 = (inx, impMode)
decode 0xca = (dex, impMode)
decode 0xc8 = (iny, impMode)
decode 0x88 = (dey, impMode)
decode 0xaa = (tax, impMode)
decode 0xa8 = (tay, impMode)
decode 0x8a = (txa, impMode)
decode 0x98 = (tya, impMode)
decode 0x9a = (txs, impMode)
decode 0xba = (tsx, impMode)
decode 0x18 = (clc, impMode)
decode 0x38 = (sec, impMode)
decode 0x58 = (cli, impMode)
decode 0x78 = (sei, impMode)
decode 0xb8 = (clv, impMode)
decode 0xd8 = (cld, impMode)
decode 0xf8 = (sed, impMode)
decode 0x10 = (bpl, impMode)
decode 0x30 = (bmi, impMode)
decode 0x50 = (bvc, impMode)
decode 0x70 = (bvs, impMode)
decode 0x90 = (bcc, impMode)
decode 0xb0 = (bcs, impMode)
decode 0xd0 = (bne, impMode)
decode 0xf0 = (beq, impMode)
decode 0x4c = (jmp, impMode)
decode 0x6c = (jpi, impMode)
decode 0x20 = (jsr, impMode)
decode 0x60 = (rts, impMode)
decode 0x00 = (brk, impMode)
decode 0x40 = (rti, impMode)
decode 0x48 = (pha, impMode)
decode 0x68 = (pla, impMode)
decode 0x08 = (php, impMode)
decode 0x28 = (plp, impMode)
decode 0xea = (nop, impMode)
decode opCode = error $ "Invalid op code: " ++ show opCode
