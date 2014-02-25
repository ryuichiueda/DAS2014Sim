import System.Environment
import System.IO
import StateTrans
import NoSeedRand
import qualified Data.ByteString.Lazy.Char8 as BS (readFile)

-- standard input: particles [x y theta[deg] w]
-- args : pos of goal x, pos of goal y, range r

type Particle = [Double]

actions = ["cw","ccw","fw"]

main :: IO () 
main = getContents >>= putStrLn . main' . readParticles

readParticles :: String -> [Particle]
readParticles cs = [ map (\w -> read w :: Double) $ words ln | ln <- lines cs ]
  
main' :: [Particle] -> String
main' ps = policy (x,y,t)
    where wsum = sum $ map (\x -> x !! 3) ps
          x = sum $ map (\x -> (x !! 3) * (x !! 0)) ps
          y = sum $ map (\y -> (y !! 3) * (y !! 1)) ps
          t1 = sum $ map (\t -> (t !! 3) * (t !! 2)) ps
          rev = map (\t -> (t !! 3) * (normalize $ (t !! 2) + 180.0) ) ps
          t2 = normalize $ (sum rev) - 180.0
          tdiff1 = sum $ map (\t -> normalize $ t1 - ((t !! 3) * (t !! 2))) ps
          tdiff2 = sum $ map (\t -> normalize $ t2 - ((t !! 3) * (t !! 2))) ps
          t = if tdiff1 < tdiff2 then t1 else t2

policy :: State -> String
policy s
  | goal == Nothing = "stay"
  | phi > t_TH      = "ccw"
  | phi < (- t_TH)  = "cw"
  | otherwise       = "fw"
    where phi = f goal
          goal = goalPos s
          t_TH = 3.0
          f (Just (ell,phi)) = phi
