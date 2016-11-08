module GameCom (demo) where

import Memory
import CPU
import PPU
import APU
import ROM

demo :: IO ()
demo = putStrLn "Nothing here right now"

step :: MachineState -> MachineState
step = id
