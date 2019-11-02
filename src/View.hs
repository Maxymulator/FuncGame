module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure (GameState s t w _) = p <> q <> r
  where 
    p :: Picture
    p = getPicture s
    q :: Picture
    q = getPicture t
    r :: Picture
    r = getPicture w