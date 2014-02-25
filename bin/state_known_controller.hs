import System.Environment
import System.IO
import StateTrans
import NoSeedRand
import qualified Data.ByteString.Lazy.Char8 as BS (readFile)

-- standard input:x y theta[deg]

type Params = (Double,Double)

main :: IO () 
main = getContents >>= putStr . policy . getPos

getPos :: String -> State
getPos cs = (x,y,t)
    where ws = words cs
          x = read (ws !! 0) ::Double
          y = read (ws !! 1) ::Double
          t = read (ws !! 2) ::Double

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
