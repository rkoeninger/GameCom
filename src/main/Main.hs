{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import Data.Default (Default(..))
import Foreign.C.Types
import GameCom (step)
import Memory (MachineState(..), Color)
import PPU (getPixel)
import SDL.Vect
import qualified SDL

width = 256
height = 240
scale = 4
frameDelayMs = 100
endDelayMs = 5000000

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
    SDL.updateWindowSurface window
    threadDelay frameDelayMs
    let (newFrame', state') = step state
    loop state' window newFrame' (n - 1)

main :: IO ()
main = do
    let state = def
    SDL.initialize [SDL.InitVideo]

    window <- SDL.createWindow
                 "GameCom"
                 SDL.defaultWindow
                 { SDL.windowInitialSize = V2 (256 * scale) (240 * scale) }
    SDL.showWindow window

    loop state window True 1

    putStrLn "Clock stopped"
    threadDelay endDelayMs

    SDL.destroyWindow window
    SDL.quit
