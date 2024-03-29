module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = playIO (InWindow "Shoot 'em Up" (round windowWidth, round windowHeight) (0, 0)) -- Or FullScreen
              green            -- Background color
              10               -- Frames per second
              initialGameState -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function