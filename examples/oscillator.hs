{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE NoImplicitPrelude #-}

module RingOscillator (main) where

import FFI
import Prelude


-- System parameters.
--
data Params = Params { alpha :: Double
                     , omega :: Double
                     , deven :: Double
                     , dodd :: Double
                     , beta :: Double
                     , sigma :: Double
                     , nosc :: Int } deriving Show

-- Update functions for system parameters used in the event handlers
-- for the parameter select lists.  Not pretty, but there's not really
-- a nicer way to do it.
--
alphaUpd, omegaUpd :: Params -> Double -> Params
devenUpd, doddUpd :: Params -> Double -> Params
betaUpd, sigmaUpd :: Params -> Double -> Params
noscUpd :: Params -> Int -> Params
alphaUpd p val = p { alpha = val }
omegaUpd p val = p { omega = val }
devenUpd p val = p { deven = val }
doddUpd p val = p { dodd = val }
betaUpd p val = p { beta = val }
sigmaUpd p val = p { sigma = val }
noscUpd p val = p { nosc = val }

-- Need this to be able to make a 'Ref Params'.
--

-- Default values for the system parameters (taken from Bridges &
-- Reich 2001).
--
defaultParams = Params { alpha = 1, omega = 1.8, deven = 0.0075, dodd = 0.0125,
                         beta = 1, sigma = 4, nosc = 5 }


-- The system state is represented as: [y, x1, ..., xN, y', x1', ..., xN']
-- where y is the position of the external forcing oscillator, x1..xN
-- are the positions of the oscillators in the ring, and y' and
-- x1'..xN' are the time derivatives.  The initial state has some
-- non-zero displacements to get us going and zero velocities.
--
initialState :: Params -> [Double]
initialState p = replicate (nosc p + 1) 1 ++ replicate (nosc p + 1) 0

-- Default initial state.
--
ic = centre defaultParams (initialState defaultParams)

-- Make data storage for scrolling graph view.
--
makeGraphData :: Int -> Int -> Fay (Double,[Buffer])
makeGraphData no size = do
  bufs <- replicateM no (newBuf size)
  return (0,bufs)


-- Calculate system right hand side, i.e. the equations that determine
-- the time derivative of the system state.
--
rhs :: Params -> [Double] -> [Double]
rhs p state = [dy] ++ dxs ++ [dy'] ++ dxs'
  where n = nosc p

        -- Split system state: Fay doesn't yet allow us to to
        -- something like ((y:xs),(y':xs')) = splitAt (n+1) state, so
        -- we just decompose things manually.
        ss = splitAt (n + 1) state
        sss = fst ss ; sss' = snd ss
        y = head sss ; xs = tail sss
        y' = head sss' ; xs' = tail sss'

        -- Time derivatives of y and the xs are immediately
        -- accessible.
        dy = y' ; dxs = xs'

        -- Forcing system right hand side.
        dy' = 0 - alpha p * (y^2 - 1) * y' - ((omega p)^2::Double) * y

        -- Right hand side for ring oscillators:

        -- Displacement differences.
        xdiffs = (head xs - (xs !! (n - 1))) : zipWith (-) (tail xs) xs

        -- Nonlinear inter-oscillator potential calculation:
        -- V(x) = 1/2 x^2 + 1/4 x^4  =>  V'(x) = x + x^3
        vprime x = x + x^3
        vp = map vprime xdiffs

        -- Differences between adjacent potential values.
        vfacs = zipWith (-) vp (tail vp) ++ [vp !! (n - 1) - head vp]

        -- Damping factors alternate between even and odd numbered
        -- oscillators.
        ds = cycle [dodd p, deven p]

        -- Calculate basic time derivative for each oscillator.
        dxstmp = zipWith3 (\d x' vf -> 0 - d * x' - beta p * vf) ds xs' vfacs

        -- Add forcing term for first oscillator in ring.
        dxs' = (head dxstmp + sigma p * y) : tail dxstmp


-- Take a single fourth order Runge-Kutta step for an autonomous
-- system.  The arguments are the right hand side function, a system
-- state and a time step.
--
rk4 :: ([Double] -> [Double]) -> [Double] -> Double -> [Double]
rk4 f yn h = zipWith5 (\y a b c d -> y+(a+2*b+2*c+d)/6) yn k1 k2 k3 k4
  where k1 = map mh $ f yn
        k2 = map mh $ f (zipWith (+) yn (map half k1))
        k3 = map mh $ f (zipWith (+) yn (map half k2))
        k4 = map mh $ f (zipWith (+) yn k3)
        mh x = h * x
        half x = 0.5 * x


-- Because the *velocities* are damped in our model, but not the
-- displacements, there tends to be a more or less linear drift in the
-- average position of the oscillators over time.  This isn't a
-- problem for displaying the positions on the ring, but it's
-- inconvenient for displaying time traces of the positions as a
-- graph.  To make this a bit easier, we centre the oscillator values
-- for graph display by subtracting the mean displacement for the ring
-- oscillators for each ring oscillator.  The forcing displacement and
-- the time derivative values are not affected by this centring, and
-- in fact none of the time derivative calculations for the system are
-- affected either, since they all depend exclusively on *differences*
-- between displacements, not the absolute values of the
-- displacements.
--
centre :: Params -> [Double] -> [Double]
centre p s = [y] ++ xsc ++ [y'] ++ xs'
  where ss = splitAt (nosc p + 1) s
        sss = fst ss ; sss' = snd ss
        y = head sss ; xs = tail sss
        y' = head sss' ; xs' = tail sss'
        m = (sum xs) / fromIntegral (length xs)
        xsc = map (\x->x-m) xs


-- Display dimensions.
--
ww, wh :: Double                -- Ring canvas dimensions.
ww = 400 ; wh = 400

gww, gwh :: Double              -- Graph canvas dimensions.
gww = 640 ; gwh = 200

-- Other display parameters.
--
dt = 0.025                      -- Integration time step.
framems = 30                    -- Milliseconds between frames.
renderrng = 6.0                 -- Nominal range for graph and ring displays.

-- Sizing for ring view elements.
--
rdotfac, rinner, rdot, fyoff :: Double
rdotfac = 0.0375
rinner = wh / (2 + 7 * rdotfac)
rdot = rdotfac * rinner
fyoff = rinner + 3 * rdot
ctr :: Point
ctr = (0.5 * ww, 5 * rdot + rinner) -- Centre point of ring display.

-- Sizing for graph view elements.
--
gwbuf, ticklen, gwwtime :: Double
gwbuf = 20                      -- Buffer at right end of graph.
ticklen = 10                    -- Graph time tick size.
gwwtime = 10                    -- Time range displayed in graph.
pxpert, pxpersamp :: Double
pxpert = gww / gwwtime          -- Pixels per time unit.
pxpersamp = dt * pxpert         -- Pixels per sample.
nsamp :: Int
nsamp = floor ((gww - gwbuf) / pxpersamp) -- Samples in graph.


-- Main entry point.  Just register an "onload" handler.
--
main :: Fay ()
main = addWindowEventListener "load" run

-- Main function: runs at load time.
--
run :: Event -> Fay Bool
run _ = do
  -- Get DOM elements and canvas contexts.
  [can,graph] <- mapM getElementById ["canvas","graph"]
  [go,stop,reset] <- mapM getElementById ["go","stop","reset"]
  sels <- mapM getElementById ["alpha","omega","deven","dodd","beta","sigma"]
  noscSel <- getElementById "nosc"
  [c,cg] <- mapM (\c -> getContext c "2d") [can,graph]

  -- Set up references to parameter values, current state, a timer
  -- identifier and the data required for rendering the graph view.
  pref <- newRef defaultParams
  xref <- newRef ic
  timerref <- newRef (Nothing :: Maybe Int)
  gd <- makeGraphData (nosc defaultParams) nsamp
  gdataref <- newRef gd

  -- Render initial ring and graph views.
  render c defaultParams ic renderrng
  renderGraph cg pref gdataref (centre defaultParams ic) renderrng

  -- Event listeners for buttons and parameter value selection.
  addEventListener go "click" $
    doGo timerref (animate c cg pref xref gdataref renderrng) framems
  addEventListener stop "click" $ doStop timerref
  addEventListener reset "click" $ doReset c cg timerref pref xref gdataref sels
  forM_ (zip sels [alphaUpd,omegaUpd,devenUpd,doddUpd,betaUpd,sigmaUpd]) $ \(s, pfn) -> do
    addEventListener s "change" $ doParamChange pref pfn
  addEventListener noscSel "change" $
    doNoscChange c cg timerref pref xref gdataref
  return False


-- If the simulation isn't currently running (marked by the timer
-- reference being Nothing), start it by causing the animation
-- function to be called at the appropriate interval.
--
doGo :: Ref (Maybe Int) -> Fay () -> Double -> Event -> Fay Bool
doGo tref anim interval _ = do
  oldtimer <- readRef tref
  case oldtimer of
    Nothing -> do
      timer <- setInterval anim interval
      writeRef tref (Just timer)
    Just _ -> return ()
  return False

-- If the simulation is running (marked by the timer reference having
-- a Just value), stop it.
--
doStop :: Ref (Maybe Int) -> Event -> Fay Bool
doStop tref _ = do
  oldtimer <- readRef tref
  case oldtimer of
    Nothing -> return ()
    Just timer -> do
      clearInterval timer
      writeRef tref Nothing
  return False

-- Stop the simulation and set everything back to the state it was at
-- the beginning, including all parameter values (the default values
-- are the middle ones out of five possibilities).
--
doReset :: Context -> Context -> Ref (Maybe Int) -> Ref Params ->
           Ref [Double] -> Ref (Double,[Buffer]) -> [Element] ->
           Event -> Fay Bool
doReset c cg tref pref xref gdataref sels e = do
  doStop tref e
  writeRef pref defaultParams
  writeRef xref ic
  gd <- makeGraphData (nosc defaultParams) nsamp
  writeRef gdataref gd
  render c defaultParams ic renderrng
  renderGraph cg pref gdataref (centre defaultParams ic) renderrng
  forM_ sels $ \s -> setSelectIndex s 2
  return False

-- Simple parameter change: just get the selected value and update the
-- parameter reference.
--
doParamChange :: Ref Params -> (Params -> Double -> Params) -> Event -> Fay Bool
doParamChange pref updfn e = do
  target <- eventTarget e
  sval <- selectValue target
  p <- readRef pref
  writeRef pref (updfn p (parseDouble sval))
  return False

-- A change in number of oscillators is a bit more complicated.  We
-- stop the simulation and reset everything to start over -- there's
-- no obvious way to derive a state with a different number of
-- oscillators from the current state.
--
doNoscChange :: Context -> Context ->
                Ref (Maybe Int) -> Ref Params ->
                Ref [Double] -> Ref (Double,[Buffer]) -> Event -> Fay Bool
doNoscChange c cg timerref pref xref gdataref e = do
  doStop timerref e
  target <- eventTarget e
  sval <- selectValue target
  let newNosc = parseInt sval
  p <- readRef pref
  let pnew = p { nosc = newNosc }
  let xnew = centre pnew (initialState pnew)
  writeRef pref pnew
  writeRef xref xnew
  gd <- makeGraphData newNosc nsamp
  writeRef gdataref gd
  render c pnew xnew renderrng
  renderGraph cg pref gdataref xnew renderrng
  return False


-- Animation function.  Take a single step of the ODE system and
-- re-render the ring and the graph views.
--
animate :: Context -> Context -> Ref Params -> Ref [Double] ->
           Ref (Double,[Buffer]) -> Double -> Fay ()
animate c cg pref xref gdataref rng = do
  p <- readRef pref
  x <- readRef xref
  let newx = rk4 (rhs p) x dt
  writeRef xref newx
  render c p newx rng
  renderGraph cg pref gdataref (centre p newx) rng


-- Render the ring view.
--
render :: Context -> Params -> [Double] -> Double -> Fay ()
render c p s rng = do
  -- Extract the forcing and the ring oscillator displacements.
  let f = head s
  let xs = take (nosc p) $ tail s

  -- Draw "furniture": ring and baseline for forcing oscillator.
  clearRect c (0,0) (ww, wh)
  beginPath c
  arc c ctr rinner 0 (2*pi)
  let fscale = rinner * 2 * pi / 5
  moveTo c $ offset ctr (-0.5*fscale,-fyoff)
  lineTo c $ offset ctr (0.5*fscale,-fyoff)
  moveTo c $ offset ctr (0, -dtickin)
  lineTo c $ offset ctr (0, -dtickout)
  setLineWidth c 2
  setStrokeStyle c "grey"
  stroke c

  -- Blobs for ring oscillators: first, forced oscillator is picked
  -- out in blue.
  forM_ [0..(nosc p)-1] (\i -> do
    let th0 = fromIntegral i * 2 * pi / fromIntegral (nosc p)
    let th = th0 + (xs !! i) / rng * 2 * pi / fromIntegral (nosc p)
    let dctr = offset ctr (rinner * sin th, -rinner * cos th)
    beginPath c
    arc c dctr rdot 0 (2*pi)
    setFillStyle c (if i == 0 then "blue" else "red")
    fill c)

  -- Blob for forcing oscillator.
  let dctr = offset ctr (f / rng * fscale, -fyoff)
  beginPath c
  arc c dctr rdot 0 (2*pi)
  setFillStyle c "grey"
  fill c
  where dtickin = fyoff - 0.5 * ticklen
        dtickout = fyoff + 0.5 * ticklen


-- Render the graph view.
--
renderGraph :: Context -> Ref Params -> Ref (Double,[Buffer]) ->
               [Double] -> Double -> Fay ()
renderGraph cg pref gdataref x rng = do
  -- Get values for parameters and time and state list.
  p <- readRef pref
  (ts,bufs) <- readRef gdataref

  -- Graph "furniture": axes and time ticks.
  setFont cg "10pt sans-serif"
  setStrokeStyle cg "grey"
  setFillStyle cg "grey"
  setLineWidth cg 2
  clearRect cg (0,0) (gww,gwh)
  beginPath cg
  moveTo cg (0,gwh/2)
  lineTo cg (gww,gwh/2)
  moveTo cg (1,0)
  lineTo cg (1,gwh)
  let ticks = takeWhile (\t -> t <= floor (ts + gwwtime)) [floor ts + 1..]
  let tickxs = map (\t -> (fromIntegral t - ts) * pxpert) ticks
  forM_ (zip ticks tickxs) $ \(t,x) -> do
    moveTo cg (x,gwh/2-ticklen/2)
    lineTo cg (x,gwh/2+ticklen/2)
    let txt = show t
    txtw <- measureText cg txt
    fillText cg txt (x-txtw/2,gwh/2+2*ticklen) Nothing
  stroke cg

  -- Add new samples to circular buffers and draw traces: grey for
  -- forcing, blue for forced ring oscillator, red for the other ring
  -- oscillators.
  setLineWidth cg 1
  newbufs <- forM (zip3 x bufs ("grey":"blue":repeat "red")) $ \(newx,buf,col) -> do
    newbuf <- bufAdd buf newx
    beginPath cg
    setStrokeStyle cg col
    y0 <- bufVal newbuf 0
    moveTo cg (0, gwh/2*(1-y0/renderrng))
    when (bufCurSize buf > 1) $ forM_ [1..bufCurSize buf-1] $ \i -> do
      y <- bufVal newbuf i
      lineTo cg (fromIntegral i * pxpersamp, gwh/2*(1-y/renderrng))
    stroke cg
    return newbuf

  -- Update the graph data reference.
  writeRef gdataref (ts+if bufCurSize (head newbufs) < nsamp then 0 else dt,newbufs)


--------------------------------------------------------------------------------
-- Utilities

zipWith5 :: (a->b->c->d->e->f) -> [a]->[b]->[c]->[d]->[e]->[f]
zipWith5 z (a:as) (b:bs) (c:cs) (d:ds) (e:es) = z a b c d e :
                                                zipWith5 z as bs cs ds es
zipWith5 _ _ _ _ _ _ = []

mapM :: (a -> Fay b) -> [a] -> Fay [b]
mapM m (x:xs) = m x >>= (\mx -> mapM m xs >>= (\mxs -> return (mx:mxs)))
mapM _ [] = return []

forM :: [a] -> (a -> Fay b) -> Fay [b]
forM (x:xs) m = m x >>= (\mx -> mapM m xs >>= (\mxs -> return (mx:mxs)))
forM [] _ = return []

replicateM :: Int -> Fay a -> Fay [a]
replicateM n x = sequence (replicate n x)

parseDouble :: String -> Double
parseDouble = ffi "parseFloat(%1)"

parseInt :: String -> Int
parseInt = ffi "parseInt(%1)"

--------------------------------------------------------------------------------
-- DOM

data Element
instance Show Element

type Size = (Int,Int)

getElementById :: String -> Fay Element
getElementById = ffi "document['getElementById'](%1)"

data Event

addWindowEventListener :: String -> (Event -> Fay Bool) -> Fay ()
addWindowEventListener = ffi "window['addEventListener'](%1,%2,false)"

addEventListener :: Element -> String -> (Event -> Fay Bool) -> Fay ()
addEventListener = ffi "%1['addEventListener'](%2,%3,false)"

setInterval :: Fay () -> Double -> Fay Int
setInterval = ffi "window['setInterval'](%1,%2)"

clearInterval :: Int -> Fay ()
clearInterval = ffi "window['clearInterval'](%1)"

print :: a -> Fay ()
print = ffi "console.log(%1)"

eventTarget :: Event -> Fay Element
eventTarget = ffi "%1['target']"

selectValue :: Element -> Fay String
selectValue = ffi "%1[%1['selectedIndex']]['value']"

setSelectIndex :: Element -> Int -> Fay ()
setSelectIndex = ffi "%1['selectedIndex']=%2"

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
-- Canvas API

data Context
instance Show Context

type Point = (Double,Double)
type Dim = (Double,Double)

getContext :: Element -> String -> Fay Context
getContext = ffi "%1.getContext(%2)"

offset :: Point -> Dim -> Point
offset (x,y) (dx,dy) = (x+dx,y+dy)

-- Basic attributes

setFillStyle :: Context -> String -> Fay ()
setFillStyle = ffi "%1['fillStyle']=%2"

setFont :: Context -> String -> Fay ()
setFont = ffi "%1['font']=%2"

setLineWidth :: Context -> Double -> Fay ()
setLineWidth = ffi "%1['lineWidth']=%2"

setStrokeStyle :: Context -> String -> Fay ()
setStrokeStyle = ffi "%1['strokeStyle']=%2"

-- Path methods

arc :: Context -> Point -> Double -> Double -> Double -> Fay ()
arc c (x,y) r beg end = arc' c x y r beg end True

arcC :: Context -> Point -> Double -> Double -> Double -> Fay ()
arcC c (x,y) r beg end = arc' c x y r beg end False

arc' :: Context -> Double -> Double -> Double -> Double -> Double ->
        Bool -> Fay ()
arc' = ffi "%1['arc'](%2,%3,%4,%5,%6,%7)"

beginPath :: Context -> Fay ()
beginPath = ffi "%1['beginPath']()"

clip :: Context -> Fay ()
clip = ffi "%1['clip']()"

closePath :: Context -> Fay ()
closePath = ffi "%1['closePath']()"

fill :: Context -> Fay ()
fill = ffi "%1['fill']()"

lineTo :: Context -> Point -> Fay ()
lineTo c (x,y) = lineTo' c x y

lineTo' :: Context -> Double -> Double -> Fay ()
lineTo' = ffi "%1['lineTo'](%2,%3)"

moveTo :: Context -> Point -> Fay ()
moveTo c (x,y) = moveTo' c x y

moveTo' :: Context -> Double -> Double -> Fay ()
moveTo' = ffi "%1['moveTo'](%2,%3)"

stroke :: Context -> Fay ()
stroke = ffi "%1['stroke']()"

-- Rectangles

clearRect :: Context -> Point -> Dim -> Fay ()
clearRect c (x,y) (w,h) = clearRect' c x y w h

clearRect' :: Context -> Double -> Double -> Double -> Double -> Fay ()
clearRect' = ffi "%1['clearRect'](%2,%3,%4,%5)"

-- Text

fillText :: Context -> String -> Point -> Maybe Double -> Fay ()
fillText c s (x,y) Nothing = fillText1 c s x y
fillText c s (x,y) (Just mw) = fillText2 c s x y mw

fillText1 :: Context -> String -> Double -> Double -> Fay ()
fillText1 = ffi "%1['fillText'](%2,%3,%4)"

fillText2 :: Context -> String -> Double -> Double -> Double -> Fay ()
fillText2 = ffi "%1['fillText'](%2,%3,%4,%5)"

measureText :: Context -> String -> Fay Double
measureText = ffi "%1['measureText'](%2)['width']"


--------------------------------------------------------------------------------
-- Circular buffers

data Array

data Buffer = Buffer { bufSize :: Int
                     , bufCurSize :: Int
                     , bufNext :: Int
                     , bufArr :: Array }

newBuf :: Int -> Fay Buffer
newBuf size = do
  arr <- newArray size
  return $ Buffer size 0 0 arr

bufAdd :: Buffer -> Double -> Fay Buffer
bufAdd (Buffer sz cursz nxt arr) x = do
  let cursz' = if cursz < sz then cursz + 1 else sz
  setArrayVal arr nxt x
  let nxt' = (nxt + 1) `rem` sz
  return $ Buffer sz cursz' nxt' arr

bufVal :: Buffer -> Int -> Fay Double
bufVal (Buffer sz cursz nxt arr) i = do
  let idx = (if cursz < sz then i else nxt + i) `rem` sz
  arrayVal arr idx >>= return

newArray :: Int -> Fay Array
newArray = ffi "new Array(%1)"

setArrayVal :: Array -> Int -> Double -> Fay ()
setArrayVal = ffi "%1[%2]=%3"

arrayVal :: Array -> Int -> Fay Double
arrayVal = ffi "%1[%2]"
