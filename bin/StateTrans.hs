module StateTrans
(  move
, strToStates
, printState
, printWorld
, actionToDelta
, landmarkPos
, goalPos
, normalize
, inGoal
, value
, Delta
, PolerPos
, State
) where

import System.Environment
import System.IO

type Xmm = Double
type Ymm = Double
type ThetaDeg = Double
type Delta = (Double,Double)
type State = (Xmm,Ymm,ThetaDeg)
type Goal = (Xmm,Ymm)
type Ellmm = Double
type PhiDeg = Double

type PolerPos = Maybe (Ellmm,PhiDeg) 

fw = 10.0
rot = 5.0
goal = (0.0,200.0)
robot_width = 50.0

printState :: State -> IO ()
printState (x,y,theta) = putStrLn . unwords $ map show [x,y,theta]

actionToDelta :: Double -> String -> Delta
actionToDelta normal_rand com
 | com == "fw" = (delta_fw,0.0)
 | com == "cw" = (0.0,- delta_rotate)
 | com == "ccw" = (0.0,delta_rotate)
 | com == "stay" = (0.0,0.0)
 | otherwise = error "Unknown action"
    where delta_rotate = pi * rot * (1.0 + 0.1*normal_rand) / 180.0
          delta_fw = fw * (1.0 + 0.1*normal_rand)

strToStates :: String -> [State]
strToStates cs = map g [ f ln | ln <- lines cs ]
    where f ln = [ read w :: Double | w <- words ln ]
          g [a,b,c] = (a,b,c)

move :: Delta -> State -> State
move (dl,dth) (x,y,th) = (x',y',normalize th')
    where rad = th / 180.0 * pi
          x' = x + dl * (cos rad)
          y' = y + dl * (sin rad)
          th' = 180.0 * ( rad + dth ) / pi

landmarkPos :: State -> PolerPos
landmarkPos (x,y,t) 
  | ell > robot_width = Just (ell,normalize phi)
  | otherwise         = Nothing
    where ell = sqrt $ x*x + y*y 
          phi = normalize $ (atan2 (-y) (-x) * 180.0 / pi ) - t

inGoal :: State -> Bool
inGoal (x,y,t) = gx*gx + gy*gy < robot_width * robot_width
    where gx = x - fst goal
          gy = y - snd goal

goalPos :: State -> PolerPos
goalPos (x,y,theta) 
  | ell > robot_width = Just (ell,normalize phi)
  | otherwise         = Nothing
    where ell = sqrt $ vx * vx + vy * vy
          phi = ((atan2 vy vx) * 180.0 / pi ) - theta
          vx = (fst goal) - x
          vy = (snd goal) - y

value :: State -> Double
value s 
  | goal == Nothing = 0.0
  | otherwise       = ell / fw + phi / rot
    where goal = goalPos s
          ell = jfst goal
          tmp = jsnd goal
          phi = if tmp > 0.0 then tmp else (- tmp)
          jfst (Just (e,p)) = e
          jsnd (Just (e,p)) = p

normalize ang
  | ang > 180.0  = normalize ( ang - 360.0 )
  | ang < -180.0 = normalize ( ang + 360.0 )
  | otherwise    = ang

printWorld :: String -> IO ()
printWorld cs = (putStrLn $ unwords ["ROBOT",head $ lines cs,show robot_width] )
              >> (putStrLn $ unwords ["GOAL",show $ fst goal,show $ snd goal] )
              >> putStrLn "LANDMARK 0 0"
