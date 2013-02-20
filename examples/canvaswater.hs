-- | Compile with: fay examples/canvaswater.hs

{-# LANGUAGE EmptyDataDecls    #-}


-- | A demonstration of Fay using the canvas element to display a
-- simple effect.

module CanvasWater (main) where

import           FFI
import           Prelude

-- | Main entry point.
main :: Fay ()
main = do
  img <- newImage
  addEventListener img "load" (start img) False
  setSrc img "haskell.png"

-- | Start the animation.
start :: Image -> Fay ()
start img = do
  canvas <- getElementById "can"
  context <- getContext canvas "2d"
  drawImage context img 0 0
  step <- newRef (0 :: Double)
  setInterval (animate context img step) 30

-- | Animate the water effect.
animate :: Context -> Image -> Ref Double -> Fay ()
animate context img step = do
  stepn <- readRef step
  setFillStyle context "rgb(255,255,255)"
  forM_ [0..imgHeight] $ \i ->
    drawImageSpecific context img
                      0 0
                      imgWidth imgHeight
                      (sin(3*(stepn+i/20.0))*(i/2.0)) (140+i)
                      imgWidth imgHeight
  writeRef step (stepn + 0.05)

imgHeight = 140
imgWidth = 200

--------------------------------------------------------------------------------
-- Elements

class Eventable a

-- | A DOM element.
data Element
instance Eventable Element

-- | Add an event listener to an element.
addEventListener :: (Eventable a) => a -> String -> Fay () -> Bool -> Fay ()
addEventListener = ffi "%1['addEventListener'](%2,%3,%4)"

-- | Get an element by its ID.
getElementById :: String -> Fay Element
getElementById = ffi "document['getElementById'](%1)"

--------------------------------------------------------------------------------
-- Images

data Image
instance Eventable Image

-- | Make a new image.
newImage :: Fay Image
newImage = ffi "new Image()"

-- | Make a new image.
setSrc :: Image -> String -> Fay ()
setSrc = ffi "%1['src'] = %2"

--------------------------------------------------------------------------------
-- Canvas

-- | A canvas context.
data Context

-- | Get an element by its ID.
getContext :: Element -> String -> Fay Context
getContext = ffi "%1['getContext'](%2)"

-- | Draw an image onto a canvas rendering context.
drawImage :: Context -> Image -> Double -> Double -> Fay ()
drawImage = ffi "%1['drawImage'](%2,%3,%4)"

-- | Draw an image onto a canvas rendering context.
--
--   Nine arguments: the element, source (x,y) coordinates, source width and
--   height (for cropping), destination (x,y) coordinates, and destination width
--   and height (resize).
--
--   context.drawImage(img_elem, sx, sy, sw, sh, dx, dy, dw, dh);
drawImageSpecific :: Context -> Image
                  -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double
                  -> Fay ()
drawImageSpecific = ffi "%1['drawImage'](%2,%3,%4,%5,%6,%7,%8,%9,%10)"

-- | Set the fill style.
setFillStyle :: Context -> String -> Fay ()
setFillStyle = ffi "%1['fillStyle']=%2"

-- | Set the fill style.
setFillRect :: Context -> Double -> Double -> Double -> Double -> Fay ()
setFillRect = ffi "%1['fillRect'](%2,%3,%4,%5)"

--------------------------------------------------------------------------------
-- Ref

-- | A mutable reference like IORef.
data Ref a

-- | Make a new mutable reference.
newRef :: a -> Fay (Ref a)
newRef = ffi "new Fay$$Ref(%1)"

-- | Replace the value in the mutable reference.
writeRef :: Ref a -> a -> Fay ()
writeRef = ffi "Fay$$writeRef(%1,%2)"

-- | Get the referred value from the mutable value.
readRef :: Ref a -> Fay a
readRef = ffi "Fay$$readRef(%1)"

--------------------------------------------------------------------------------
-- Misc

-- | Alert using window.alert.
alert :: a -> Fay ()
alert = ffi "window['alert'](%1)"

-- | Alert using window.alert.
print :: Double -> Fay ()
print = ffi "console['log'](%1)"

-- | Alert using window.alert.
log :: String -> Fay ()
log = ffi "console['log'](%1)"

-- | Alert using window.alert.
setInterval :: Fay () -> Double -> Fay ()
setInterval = ffi "window['setInterval'](%1,%2)"
