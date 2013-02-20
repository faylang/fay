{-
    (By Chris Smith)
    Reimplementation of gloss (minus bitmaps) in terms of Fay.  This is a proof
    of concept that Fay is in a state where it can support use cases like
    gloss-web on the client.

    TODO:
    - Fix the problem with unary negation
    - Implement support for events and game mode

    To try it out, skip the boilerplate section at the top; in the final
    implementation, it will eventually be moved to a different module and
    imported.  Change the definition of go to one of drawIt, animateIt, or
    simulateIt to try the various modes.
-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE EmptyDataDecls    #-}

module CodeWorld where

import Prelude
import FFI


data Element

getElementById :: String -> Fay Element
getElementById = ffi "document['getElementById'](%1)"

focusElement :: Element -> Fay ()
focusElement = ffi "%1.focus()"

data Event

addEventListener :: String -> Bool -> (Event -> Fay Bool) -> Fay ()
addEventListener = ffi "window['addEventListener'](%1,%3,%2)"

--getEventKeyCode :: Event -> Fay String
--getEventKeyCode = ffi "%1['keyCode']"

getEventMouseButton :: Event -> Fay Int
getEventMouseButton = ffi "%1['button']"

data Timer

setInterval :: Double -> Fay () -> Fay Timer
setInterval = ffi "window['setInterval'](%2,%1)"

data Ref a

newRef :: a -> Fay (Ref a)
newRef = ffi "new Fay$$Ref(%1)"

writeRef :: Ref a -> a -> Fay ()
writeRef = ffi "Fay$$writeRef(%1,%2)"

readRef :: Ref a -> Fay a
readRef = ffi "Fay$$readRef(%1)"

currentTimeMillis :: Fay Double
currentTimeMillis = ffi "(Date.now?Date.now():new Date().getTime())"

data Context

getContext :: Element -> String -> Fay Context
getContext = ffi "%1['getContext'](%2)"

clearRect :: Context -> Double -> Double -> Double -> Double -> Fay ()
clearRect = ffi "%1['clearRect'](%2,%3,%4,%5)"

save :: Context -> Fay ()
save = ffi "%1['save']()"

restore :: Context -> Fay ()
restore = ffi "%1['restore']()"

canvasTranslate :: Context -> Double -> Double -> Fay ()
canvasTranslate = ffi "%1['translate'](%2,%3)"

canvasScale :: Context -> Double -> Double -> Fay ()
canvasScale = ffi "%1['scale'](%2,%3)"

transform :: Context
          -> Double -> Double -> Double -> Double -> Double -> Double
          -> Fay ()
transform = ffi "%1['transform'](%2,%3,%4,%5,%6,%7)"

setTextAlign :: Context -> String -> Fay ()
setTextAlign = ffi "%1['textAlign']=%2"

setTextBaseline :: Context -> String -> Fay ()
setTextBaseline = ffi "%1['textBaseline']=%2"

setLineWidth :: Context -> Double -> Fay ()
setLineWidth = ffi "%1['lineWidth']=%2"

setFont :: Context -> String -> Fay ()
setFont = ffi "%1['font']=%2"

setStrokeStyle :: Context -> String -> Fay ()
setStrokeStyle = ffi "%1['strokeStyle']=%2"

setFillStyle :: Context -> String -> Fay ()
setFillStyle = ffi "%1['fillStyle']=%2"

beginPath :: Context -> Fay ()
beginPath = ffi "%1['beginPath']()"

closePath :: Context -> Fay ()
closePath = ffi "%1['closePath']()"

fill :: Context -> Fay ()
fill = ffi "%1['fill']()"

stroke :: Context -> Fay ()
stroke = ffi "%1['stroke']()"

fillText :: Context ->String -> Double -> Double -> Fay ()
fillText = ffi "%1['fillText'](%2,%3,%4)"

moveTo :: Context -> Double -> Double -> Fay ()
moveTo = ffi "%1['moveTo'](%2,%3)"

lineTo :: Context -> Double -> Double -> Fay ()
lineTo = ffi "%1['lineTo'](%2,%3)"

canvasArc :: Context -> Double -> Double -> Double -> Double -> Double -> Bool -> Fay ()
canvasArc = ffi "%1['arc'](%2,%3,%4,%5,%6,%7)"

-- Special functions defined in codeworld.js

getSpecialKey :: Event -> Fay String
getSpecialKey = ffi "window.getSpecialKey(%1)"

getPressedKey :: Event -> Fay String
getPressedKey = ffi "window.getPressedKey(%1)"

getReleasedKey :: Event -> Fay String
getReleasedKey = ffi "window.getReleasedKey(%1)"

stopEvent :: Event -> Fay ()
stopEvent = ffi "window.stopEvent(%1)"

mouseToElementX :: Event -> Fay Double
mouseToElementX = ffi "window.mouseToElementX(%1)"

mouseToElementY :: Event -> Fay Double
mouseToElementY = ffi "window.mouseToElementY(%1)"

type Transform = (Double, Double, Double, Double, Double, Double)

translateTransform :: Double -> Double -> Transform -> Transform
translateTransform x y (a,b,c,d,e,f) =
    (a, b, c, d, a * x + c * y + e, b * x + d * y + f)

scaleTransform :: Double -> Double -> Transform -> Transform
scaleTransform x y (a,b,c,d,e,f) =
    (x*a, x*b, y*c, y*d, e, f)

rotateTransform :: Double -> Transform -> Transform
rotateTransform r (a,b,c,d,e,f) = let th = r * pi / 180 in
    (a * cos th + c * sin th,
     b * cos th + d * sin th,
     c * cos th - a * sin th,
     d * cos th - b * sin th,
     e, f)

withTransform :: Context -> Transform -> Fay () -> Fay ()
withTransform ctx (a,b,c,d,e,f) action = do
    save ctx
    transform ctx a b c d e f
    beginPath ctx
    action
    restore ctx

data Color = RGBA Double Double Double Double

white, black :: Color
white = RGBA 1 1 1 1
black = RGBA 0 0 0 1

red, green, blue, cyan, magenta, yellow :: Color
red        = RGBA 1 0 0 1
green      = RGBA 0 1 0 1
blue       = RGBA 0 0 1 1
yellow     = RGBA 1 1 0 1
cyan       = RGBA 0 1 1 1
magenta    = RGBA 1 0 1 1

orange, rose, chartreuse, aquamarine, violet, azure :: Color
orange     = RGBA 1.0 0.5 0.0 1
rose       = RGBA 1.0 0.0 0.5 1
chartreuse = RGBA 0.5 1.0 0.0 1
aquamarine = RGBA 0.0 1.0 0.5 1
violet     = RGBA 0.5 0.0 1.0 1
azure      = RGBA 0.0 0.5 1.0 1

light :: Color -> Color
light (RGBA r g b a) = RGBA
    (min 1 (r + 0.2))
    (min 1 (g + 0.2))
    (min 1 (b + 0.2))
    a

dark :: Color -> Color
dark (RGBA r g b a) = RGBA
    (max 0 (r - 0.2))
    (max 0 (g - 0.2))
    (max 0 (b - 0.2))
    a

gray, grey :: Double -> Color
gray = grey
grey k = RGBA k k k 1

type Point = (Double, Double)
type Vector = Point

data Picture = Polygon [Point]
             | Line [Point]
             | ThickArc Double Double Double Double
             | Text String
             | Color Color Picture
             | Translate Double Double Picture
             | Scale Double Double Picture
             | Rotate Double Picture
             | Pictures [Picture]

blank :: Picture
blank = Pictures []

polygon :: [Point] -> Picture
polygon = Polygon

line :: [Point] -> Picture
line = Line

thickArc :: Double -> Double -> Double -> Double -> Picture
thickArc = ThickArc

arc :: Double -> Double -> Double -> Picture
arc b e r = thickArc b e r 0

circle :: Double -> Picture
circle = arc 0 360

circleSolid :: Double -> Picture
circleSolid r = thickCircle (r/2) r

thickCircle :: Double -> Double -> Picture
thickCircle = thickArc 0 360

text :: String -> Picture
text = Text

color :: Color -> Picture -> Picture
color = Color

translate :: Double -> Double -> Picture -> Picture
translate = Translate

scale :: Double -> Double -> Picture -> Picture
scale = Scale

rotate :: Double -> Picture -> Picture
rotate = Rotate

pictures :: [Picture] -> Picture
pictures = Pictures

rectangleSolid :: Double -> Double -> Picture
rectangleSolid w h = polygon [
    (0-w/2, 0-h/2), (w/2, 0-h/2), (w/2, h/2), (0-w/2, h/2)
    ]

rectangleWire :: Double -> Double -> Picture
rectangleWire w h = line [
    (0-w/2, 0-h/2), (w/2, 0-h/2), (w/2, h/2), (0-w/2, h/2), (0-w/2, 0-h/2)
    ]

pathFromPoints :: Context -> [Point] -> Fay ()
pathFromPoints _   [] = return ()
pathFromPoints ctx ((sx,sy):ps) = do
    moveTo ctx sx sy
    forM_ ps $ \(x,y) -> lineTo ctx x y

drawPicture :: Context -> Transform -> Picture -> Fay ()
drawPicture ctx t (Polygon ps) = do
    withTransform ctx t $ pathFromPoints ctx ps
    fill ctx

drawPicture ctx t (Line ps) = do
    withTransform ctx t $ pathFromPoints ctx ps
    stroke ctx

drawPicture ctx t (ThickArc b e r w) = do
    save ctx
    withTransform ctx t $ do
        when (r > 0) $ canvasArc ctx 0 0 r (b*pi/180) (e*pi/180) False
        closePath ctx
    when (w > 0) $ setLineWidth ctx w
    stroke ctx
    restore ctx

drawPicture ctx t (Text txt) =
    withTransform ctx t $ do
        canvasScale ctx 1 (0-1)
        fillText ctx txt 0 0

drawPicture ctx t (Color (RGBA r g b a) p) = do
    let str = "rgba(" ++ show (r * 100) ++ "%,"
                      ++ show (g * 100) ++ "%,"
                      ++ show (b * 100) ++ "%,"
                      ++ show a ++ ")"
    save ctx
    setStrokeStyle ctx str
    setFillStyle ctx str
    drawPicture ctx t p
    restore ctx

drawPicture ctx t (Translate x y p) = drawPicture ctx (translateTransform x y t) p
drawPicture ctx t (Scale x y p)     = drawPicture ctx (scaleTransform x y t) p
drawPicture ctx t (Rotate r p)      = drawPicture ctx (rotateTransform r t) p
drawPicture ctx t (Pictures ps)     = mapM_ (drawPicture ctx t) ps

withCanvas :: (Element -> Context -> Fay ()) -> Fay ()
withCanvas go = addEventListener "load" False $ const $ do
    canvas <- getElementById "canvas"
    ctx    <- getContext canvas "2d"
    go canvas ctx
    return False

drawOn :: Context -> Picture -> Fay ()
drawOn ctx pic = do
    clearRect ctx 0 0 500 500
    save ctx
    canvasTranslate ctx 250 250
    canvasScale ctx 1 (0-1)
    setTextAlign ctx "left"
    setTextBaseline ctx "alphabetic"
    setLineWidth ctx 0
    setFont ctx "100px Times Roman"
    drawPicture ctx (1,0,0,1,0,0) pic
    restore ctx

displayInCanvas :: Picture -> Fay ()
displayInCanvas pic = withCanvas $ \ canvas ctx -> drawOn ctx pic

animateInCanvas :: (Double -> Picture) -> Fay ()
animateInCanvas anim = withCanvas $ \ canvas ctx -> do
    startTime <- currentTimeMillis
    setInterval 30 $ do
        currentTime <- currentTimeMillis
        let t = (currentTime - startTime) / 1000
        drawOn ctx (anim t)
    return ()

data SimState a = SimState a

withSimState :: (a -> a) -> Ref (SimState a) -> Fay a
withSimState f ref = do
    SimState val <- readRef ref
    let newVal = f val
    writeRef ref (SimState newVal)
    return newVal

-- XXX: This is a hack to pretend to have a decent purely functional
-- random number generator.  It should be replaced by a correct
-- implementation as soon as possible.

unsafeRand :: Double -> Double -> Double
unsafeRand = ffi "Math.random()*%2+%1"

data StdGen = StdGen

newStdGen :: Fay StdGen
newStdGen = return StdGen

splitR :: StdGen -> (StdGen, StdGen)
splitR g = (g, g)

randomR :: (Double, Double) -> StdGen -> (Double, StdGen)
randomR (lo, hi) g = (unsafeRand lo (hi - lo), g)

simulateInCanvas :: (StdGen -> a)
                 -> (Double -> a -> a)
                 -> (a -> Picture)
                 -> Fay ()
simulateInCanvas i s d = withCanvas $ \ canvas ctx -> do
    startTime <- currentTimeMillis
    g <- newStdGen
    valueRef <- newRef (SimState (i g))
    timeRef <- newRef startTime
    setInterval 30 $ do
        lastTime     <- readRef timeRef
        currentTime  <- currentTimeMillis
        let dt     = (currentTime - lastTime) / 1000
        val <- withSimState (s dt) valueRef
        writeRef timeRef currentTime
        drawOn ctx (d val)
    return ()

data GameEvent = KeyPressEvent String
               | KeyReleaseEvent String
               | MousePressEvent Int Point
               | MouseReleaseEvent Int Point
               | MouseMoveEvent Point
  deriving Show
playInCanvas :: (StdGen -> a)
             -> (Double -> a -> a)
             -> (GameEvent -> a -> a)
             -> (a -> Picture)
             -> Fay ()
playInCanvas i s e d = withCanvas $ \ canvas ctx -> do
    startTime <- currentTimeMillis
    g <- newStdGen
    valueRef <- newRef (SimState (i g))
    timeRef <- newRef startTime
    addEventListener "keydown" False $ \ev -> do
        k <- getSpecialKey ev
        if k == "None" then return True else do
            withSimState (e (KeyPressEvent k)) valueRef
            stopEvent ev
            return False
    addEventListener "keypress" False $ \ev -> do
        k <- getPressedKey ev
        if k == "None" then return True else do
            withSimState (e (KeyPressEvent k)) valueRef
            stopEvent ev
            return False
    addEventListener "keyup" False $ \ev -> do
        k <- getReleasedKey ev
        if k == "None" then return True else do
            withSimState (e (KeyReleaseEvent k)) valueRef
            stopEvent ev
            return False
    addEventListener "mousedown" False $ \ev -> do
        focusElement canvas
        x <- mouseToElementX ev
        y <- mouseToElementY ev
        if abs x > 250 || abs y > 250 then return True else do
            b <- getEventMouseButton ev
            withSimState (e (MousePressEvent b (x,y))) valueRef
            stopEvent ev
            return False
    addEventListener "mouseup" False $ \ev -> do
        x <- mouseToElementX ev
        y <- mouseToElementY ev
        if abs x > 250 || abs y > 250 then return True else do
            b <- getEventMouseButton ev
            withSimState (e (MouseReleaseEvent b (x,y))) valueRef
            stopEvent ev
            return False
    addEventListener "mousemove" False $ \ev -> do
        x <- mouseToElementX ev
        y <- mouseToElementY ev
        if abs x > 250 || abs y > 250 then return True else do
            withSimState (e (MouseMoveEvent (x,y))) valueRef
            stopEvent ev
            return False
    setInterval 30 $ do
        lastTime     <- readRef timeRef
        currentTime  <- currentTimeMillis
        let dt     = (currentTime - lastTime) / 1000
        val <- withSimState (s dt) valueRef
        writeRef timeRef currentTime
        drawOn ctx (d val)
    return ()
