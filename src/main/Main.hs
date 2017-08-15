{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import qualified Data.ByteString as BS
import Data.Default (Default(..))
import Foreign.C.Types
import GameCom (step)
import Memory (MachineState(..), Color)
import PPU (getPixel)
import ROM (parseROM)
import SDL.Vect
import qualified SDL
import System.Directory (getCurrentDirectory)

width = 256
height = 240
scale = 4
frameDelayMs = 0
endDelayMs = 5000000
romName = "full_palette"

draw :: MachineState -> SDL.Window -> IO ()
draw state window =
    SDL.getWindowSurface window >>=
        (\ surface ->
            forM_ [0..(width - 1)] (\ x ->
                forM_ [0..(height - 1)] (\ y ->
                  do
                    let (r, g, b) = getPixel (x, y) state
                    let color = V4 r g b maxBound
                    let area = SDL.Rectangle
                                 (P (V2 (x * scale) (y * scale)))
                                 (V2 scale scale)
                    SDL.surfaceFillRect surface (Just area) color)))

loop :: MachineState -> SDL.Window -> Bool -> Int -> IO ()
loop _ _ _ 0 = return ()
loop state window newFrame n = do
    if newFrame
        then draw state window
        else return ()
    if n `mod` 60 == 0
        then putStrLn $ "Frames remaining: " ++ show n
        else return ()
    SDL.updateWindowSurface window
    threadDelay frameDelayMs
    let (newFrame', state') = step state
    loop state' window newFrame' (n - 1)

fromRight (Right x) = x
fromRight (Left x) = error $ "ROM load failure: " ++ x

main :: IO ()
main = do
    pwd <- getCurrentDirectory
    putStrLn $ "Working Directory: " ++ pwd
    romBytes <- BS.readFile $ "roms/" ++ romName ++ ".nes"
    
    let state = def { rom = fromRight $ parseROM romBytes }
    SDL.initialize [SDL.InitVideo]

    window <- SDL.createWindow
                 "GameCom"
                 SDL.defaultWindow
                 { SDL.windowInitialSize = V2 (256 * scale) (240 * scale) }
    SDL.showWindow window

    loop state window True 10000

    putStrLn "Clock stopped"
    threadDelay endDelayMs

    SDL.destroyWindow window
    SDL.quit
