module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure (GameState s t w) = p <> q <> r
  where 
    p :: Picture
    p = Translate -100.0 -100.0 (Text show s)
    q :: Picture
    q = Translate  100.0 -100.0 (Text show t)
    r :: Picture
    r = getPicture w