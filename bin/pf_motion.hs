import System.Environment
import System.IO
import StateTrans
import NoSeedRand
import qualified Data.ByteString.Lazy.Char8 as BS (readFile)
-- standard :: a record: input:x y theta[deg] weight
-- -- args : action "fw..."

type Particle = [Double] -- x y theta w
type Move = (Double,Double)

main :: IO () 
main = do as <- getArgs
          rands <- BS.readFile "/dev/random"
          getContents >>= main' (as !! 0) (normalRands rands) (uniformRands rands) . readParticles 

readParticles :: String -> [Particle]
readParticles cs = [ map (\w -> read w :: Double) $ words ln | ln <- lines cs ]

main' :: String -> [Double] -> [Double] -> [Particle] -> IO ()
main' a rs urs ps = putStr $ unlines [ unwords $ map show p | p <- nps ]
    where new_samples = resampling a rs ps
          nps = numAdjust urs old_num new_num new_samples
          old_num = length ps
          new_num = length new_samples

numAdjust :: [Double] -> Int -> Int -> [Particle] -> [Particle]
numAdjust urs target newnum ps
  | target > newnum  = ps ++ add_ps
  | target == newnum = ps
  | otherwise        = take target $ shuffle ps 
    where add_ps = addRandomSamples w (take (addnum*3) urs)
          w = 1.0 / (fromIntegral target :: Double)
          addnum = target - newnum
{--
    where add_ps = addRandomSamples w c $ take ((target - newnum)*3) urs
          w = 1.0 / (fromIntegral target :: Double)
          c = center ps
--}
--
addRandomSamples :: Double -> [Double] -> [Particle]
addRandomSamples _ [] = []
addRandomSamples new_w (a:b:c:rs) = [x,y,t,new_w] : addRandomSamples new_w rs
    where x = 4000.0 * a - 2000.0
          y = 4000.0 * b - 2000.0
          t = 180.0 * c - 180.0
{--
    where new_x = x + 10.0 * a - 5.0
          new_y = y + 10.0 * b - 5.0
          new_t = t + 10.0 * c - 5.0
--}

shuffle :: [Particle] -> [Particle]
shuffle ps = map snd $ eps ++ ops 
    where ps' = zip [1..] ps
          ops = filter (\p -> (fst p) `mod` 2 == 1 ) ps'
          eps = filter (\p -> (fst p) `mod` 2 == 0 ) ps'

{--
addRandomSamples :: Double -> State -> [Double] -> [Particle]
addRandomSamples _     (x,y,t) []         = []
addRandomSamples new_w (x,y,t) (a:b:c:rs) = [new_x,new_y,new_t,new_w] : addRandomSamples new_w (x,y,t) rs
    where new_x = x + 100.0 * a - 50.0
          new_y = y + 100.0 * b - 50.0
          new_t = t + 100.0 * c - 50.0
--}

center :: [Particle] -> State
center ps = (x,y,t)
    where x = f ps 0
          y = f ps 1
          t = f ps 2
          f [] _ = 0.0
          f (p:ps) pos = (p !! pos) * (p !! 3) + f ps pos

resampling :: String -> [Double] -> [Particle] -> [Particle]
resampling a rs ps = resampling' a (quota ps rs) new_w
    where wsum = sum $ map (\p -> p !! 3) ps
          num = length ps
          fixnum p = round $ (p !! 3) / wsum * (fromIntegral num :: Double)
          quota [] rs     = []
          quota (p:ps) rs = (p,take (fixnum p) rs) : quota ps (drop (fixnum p) rs)
          new_w = 1.0 / (fromIntegral num :: Double)

resampling' :: String -> [(Particle,[Double])] -> Double -> [Particle]
resampling' a []     new_w = []
resampling' a (p:ps) new_w = concat $ map (resampling'' a new_w) ps

resampling'' :: String -> Double -> (Particle,[Double]) -> [Particle]
resampling'' a new_w ([x,y,t,w],rs) = [ resampling''' a [x,y,t,new_w] r | r <- rs ]

resampling''' :: String -> Particle -> Double -> Particle
resampling''' a [x,y,t,w] r = f p
    where p = move (actionToDelta r a) (x,y,t)
          f (x,y,t) = [x,y,t,w]
