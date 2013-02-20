-- By Chris Smith

{-# LANGUAGE NoImplicitPrelude #-}

module MyDrawing where

import Prelude
import FFI
import CodeWorld

main = playInCanvas initial step event draw

data World = World {
    stars     :: [(Double, Double, Double)],
    asts      :: [(Point, Vector)],
    ship      :: (Point, Vector),
    direction :: Double,
    left      :: Double,
    right     :: Double,
    thrust    :: Double,
    energy    :: Double,
    score     :: Double,
    lastScore :: Double,
    maxScore  :: Double,
    savedGen  :: StdGen
    }

initial g = initialWith 0 0 g

initialWith m l g0 =
  case splitR g0 of
    (genStars, g1) -> case splitR g1 of
      (genAsts,  g2) ->
        World {
            stars     = take 40 (makeStars genStars),
            asts      = take 20 (makeAsts  genAsts),
            ship      = ((0,0), (0,0)),
            direction = 0,
            left      = 0,
            right     = 0,
            thrust    = 0,
            energy    = 1,
            score     = 0,
            maxScore  = m,
            lastScore = l,
            savedGen  = g2
            }

makeStars g0 = case randomR (-250, 250) g0 of
  (x, g1) -> case randomR (-250, 250) g1 of
    (y, g2) -> case randomR (   1,   3) g2 of
      (r, g3) -> (x,y,r) : makeStars g3

makeAsts  g0 = case randomR (-250, 250) g0 of
  (x,  g1) -> case randomR (-250, 250) g1 of
    (y,  g2) -> case randomR ( -30,  30) g2 of
      (vx, g3) -> case randomR ( -30,  30) g3 of
        (vy, g4) -> ((x,y), (vx, vy)) : makeAsts g4

effective w x | energy w > 0 = x w
              | otherwise    = 0

lost w = any (collision (ship w)) (asts w)
    where collision ((x1,y1),_) ((x2,y2),_) = (((x2-x1)^2::Double) + ((y2-y1)^2::Double)) < 1764

step dt w = if lost w
    then initialWith (maxScore w) (if score w < 1 then lastScore w else score w) (savedGen w)
    else w {
        asts      = map (stepBody dt) (asts w),
        ship      = stepThrust dt (stepBody dt (ship w)) (effective w thrust) (direction w),
        direction = stepDir    dt (direction w) (left w) (right w),
        energy    = fence 0 1 (energy w + dt * (0.5 * (1 - thrust w) - 1.0 * thrust w)),
        score     = score w + dt,
        maxScore  = max (maxScore w) (score w)
        }

fence lo hi v = max 0 (min hi v)

stepThrust dt ((x,y), (sx,sy)) th dir = ((x,y), (sx', sy'))
    where sx' = sx + th * (-30) * sin (dir * pi / 180) * dt
          sy' = sy + th *   30  * cos (dir * pi / 180) * dt

stepDir dt dir l r = dir + l * 90 * dt - r * 90 * dt

stepBody dt ((x,y),(sx,sy)) = ((wrap (x + sx * dt), wrap (y + sy * dt)), (sx, sy))
  where wrap k | k <= (-300) = k + 600
               | k >=   300  = k - 600
               | otherwise   = k

draw w = pictures [
    rectangleSolid 500 500,
    drawStars (stars w),
    drawAsts (asts w),
    drawShip (ship w) (direction w) (effective w thrust),
    drawEnergyBar (energy w),
    drawScoreBar (score w) (lastScore w) (maxScore w)
    ]

drawStars ss = pictures [
    color (gray 0.5) (translate x y (circleSolid r ))
        | (x,y,r)   <- ss
    ]

drawAsts  as = pictures [
    color (light red) (translate x y (circleSolid 30))
        | ((x,y),_) <- as
    ]

drawShip ((x,y),_) dir th = translate x y (rotate dir (pictures [
    color (gray 0.2) (circle 12),
    color cyan   (polygon [(-9, -8), (9, -8), ( 0, 12) ]),
    if th > 0 then color orange (polygon [( -8, -8), (-10, -11), (10, -11), (8, -8)])
              else blank
    ]))

drawEnergyBar e = color yellow $ translate 0 (-230) $ rectangleSolid (400 * e) 15

drawScoreBar s l m = pictures [
  color blue $ translate 0 230 $ rectangleSolid 500 15,
  color white $ translate (-200) 225 $ scale 0.2 0.15 $ text $ "Score: " ++ fmtScore s,
  color white $ translate ( -25) 225 $ scale 0.2 0.15 $ text $ "Last: " ++ fmtScore l,
  color white $ translate ( 150) 225 $ scale 0.2 0.15 $ text $ "Max: " ++ fmtScore m
  ]

fmtScore :: Double -> String
fmtScore s = show (floor (10 * s))

event (KeyPressEvent   "Up")    w = w { thrust = 1 }
event (KeyReleaseEvent "Up")    w = w { thrust = 0 }
event (KeyPressEvent   "Left")  w = w { left   = 1 }
event (KeyReleaseEvent "Left")  w = w { left   = 0 }
event (KeyPressEvent   "Right") w = w { right  = 1 }
event (KeyReleaseEvent "Right") w = w { right  = 0 }
event _                         w = w
