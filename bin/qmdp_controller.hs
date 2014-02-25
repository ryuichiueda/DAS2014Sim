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
main = do rands <- BS.readFile "/dev/random"
          getContents >>= putStrLn . policy (normalRands rands) . readParticles

readParticles :: String -> [Particle]
readParticles cs = [ map (\w -> read w :: Double) $ words ln | ln <- lines cs ]
  
policy :: [Double] -> [Particle] -> String
policy rs ps = fst $ getMin act_values
    where act_values = zip actions (map (actionEval rs ps) actions)

getMin :: [(String,Double)] -> (String,Double)
getMin (a:[])   = a
getMin (a:b:as) = if snd a < snd b then getMin (a:as) else getMin (b:as)

actionEval :: [Double] -> [Particle] -> String -> Double
actionEval rs ps a = valueSum ps'
    where ps' = moveParticles (getDeltas rs a) ps

valueSum :: [Particle] -> Double
valueSum [] = 0.0
valueSum ([x,y,t,w]:ps) = w * value (x,y,t) + valueSum ps

getDeltas :: [Double] -> String -> [Delta]
getDeltas (r:rs) a = actionToDelta r a : getDeltas rs a

moveParticles :: [Delta] -> [Particle] -> [Particle]
moveParticles _      []             = []
moveParticles (d:ds) ([x,y,t,w]:ps) = f w (move d (x,y,t)) : moveParticles ds ps
    where f w (x,y,t) = [x,y,t,w]

