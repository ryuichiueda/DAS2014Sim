import System.Environment
import System.IO
import StateTrans
import NoSeedRand
import Data.List
import qualified Data.ByteString.Lazy.Char8 as BS (readFile)

-- standard :: a record: input:x y theta[deg] weight
-- args : th sensor value

type Particle = [Double] -- x y theta w
type Param = [Double]

main :: IO () 
main = do as <- getArgs
          rands <- BS.readFile "/dev/random"
          getContents >>= main' (uniformRands rands) (normalRands rands) (readParam as) . readParticles

readParam :: [String] -> [Double]
readParam [] = []
readParam (a:args) = (read a :: Double ) : readParam args
        
readParticles :: String -> [Particle]
readParticles cs = [ map (\w -> read w :: Double) $ words ln | ln <- lines cs ]

main' :: [Double] -> [Double] -> [Double] -> [Particle] -> IO ()
main' urs rs (th:ell:phi:_) ps 
  | wsum < th  = putStr . unlines $ map toStr $ sensorReset urs rs num ell phi w
  | otherwise  = putStr . unlines $ map toStr $ toOne ps
    where wsum = wSum ps
          w = 1.0 / (fromIntegral num :: Double)
          num = length ps
main' urs rs _ ps =  putStr . unlines $ map toStr $ toOne ps

toOne :: [Particle] -> [Particle]
toOne ps = map f ps
    where wsum = wSum ps
          f [x,y,t,w] = [x,y,t,w/wsum]

toStr :: Particle -> String
toStr p = unwords $ map (\a -> show a) p

wSum :: [Particle] -> Double
wSum (p:[]) = p !! 3
wSum (p:ps) = (p !! 3) + wSum ps

sensorReset :: [Double] -> [Double] -> Int -> Double -> Double -> Double -> [Particle]
sensorReset _ _ 0 _ _ _ = []
sensorReset (ur:urs) (r:r2:rs) num ell phi w = [fst xy,snd xy,theta,w] : sensorReset urs rs (num-1) ell phi w
    where xy = choiceXY ur r ell
          theta = choiseTheta r2 phi xy

choiceXY :: Double -> Double -> Double ->  (Double,Double)
choiceXY ur r ell = (x,y)
    where ang_rad = ur * pi * 2 - 180.0
          ellnoise = ell * (1.0 + 0.1 * r )
          x = ellnoise * cos (ang_rad)
          y = ellnoise * sin (ang_rad)

choiseTheta :: Double -> Double -> (Double,Double) -> Double
choiseTheta r phi (x,y) = normalize $ (5.0 * r) + alpha - phi + 180.0
--choiseTheta r phi (x,y) = normalize (alpha - phi + pi)
--choiseTheta r phi (x,y) = 0.0
    where alpha = (atan2 y x ) / pi * 180.0
