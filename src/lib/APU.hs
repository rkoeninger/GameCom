module APU where

import Data.Word (Word8, Word16)

pulseWaveforms :: [Word8]
pulseWaveforms = [0x40, 0x60, 0x78, 0x9f]

lengthCounters :: [Word8]
lengthCounters = [
    10, 254, 20,  2, 40,  4, 80,  6, 160,  8, 60, 10, 14, 12, 26, 14,
    12,  16, 24, 18, 48, 20, 96, 22, 192, 24, 72, 26, 16, 28, 32, 30]

triangleWaveform :: [Word8]
triangleWaveform = [
    15, 14, 13, 12, 11, 10,  9,  8,  7,  6,  5,  4,  3,  2,  1,  0,
     0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15]

-- TODO: PAL
noisePeriods :: [Word16]
noisePeriods = [
    4, 8, 16, 32, 64, 96, 128, 160, 202, 254, 380, 508, 762, 1016, 2034, 4068]
