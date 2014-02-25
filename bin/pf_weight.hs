import System.Environment
import System.IO
import StateTrans
import qualified NoSeedRand as R (uniformRands)
import Data.List
import qualified Data.ByteString.Lazy.Char8 as BS (readFile)

-- standard :: a record: input:x y theta[deg] weight
-- args : goal x, goal y, range

type Particle = [Double] -- x y theta w

main :: IO () 
main = do as <- getArgs
          rands <- BS.readFile "/dev/random"
          getContents >>= main' (R.uniformRands rands) (readTh as) . readParticles

readTh :: [String] -> Double
readTh (a:args) = read a :: Double
        
readParticles :: String -> [Particle]
readParticles cs = [ map (\w -> read w :: Double) $ words ln | ln <- lines cs ]

main' :: [Double] -> Double -> [Particle] -> IO ()
main' rs th ps 
  | wsum < th  = putStr . unlines $ map g $ expReset rs ps
  | otherwise  = putStr . unlines $ map f ps
    where wsum = wSum ps
          f [x,y,t,w] = unwords $ map show [x,y,t,w/wsum]
          g [x,y,t,w] = unwords $ map show [x,y,t,w]

wSum :: [Particle] -> Double
wSum (p:[]) = p !! 3
wSum (p:ps) = (p !! 3) + wSum ps

expReset :: [Double] -> [Particle] -> [Particle]
expReset rs ps = genParticles rs (1.0/(fromIntegral num :: Double)) num xrange yrange trange
    where xs = map (\p -> p !! 0) ps
          ys = map (\p -> p !! 1) ps
          ts = map (\p -> p !! 2) ps
          ws = map (\p -> p !! 3) ps
          xrange = (getMin xs,getMax xs)
          yrange = (getMin ys,getMax ys)
          trange = (minimum nts,maximum nts)
          getMin as = max (-2000.0) (2 * (minimum as))
          getMax as = min 2000.0 (2 * (maximum as))
          theta_mode = fromInteger (fst $ mode (0,0) countTheta) :: Double
          countTheta = drop 1 $ countElems (-1000,1) (sort $ map round ts)
          nts = [ theta_mode + 2*(normalize $ t - theta_mode) | t <- ts ]
          num = length ps

genParticles :: [Double] -> Double -> Int -> (Double,Double) -> (Double,Double) -> (Double,Double) -> [Particle]
genParticles _  _ 0   _ _ _ = []
genParticles (e1:e2:e3:rs) w num xr yr tr = [f e1 xr,f e2 yr,normalize $ f e3 tr,w] : genParticles rs w (num-1) xr yr tr
    where f r (a,b) = a + (b - a)*r

mode :: (a,Int) -> [(a,Int)] -> (a,Int)
mode ans []   = ans
mode ans (a:as) = if (snd ans) > (snd a) then mode ans as else mode a as

countElems :: (Eq a) => (a,Int) -> [a] -> [(a,Int)]
countElems (x,c) [] = (x,c) : []
countElems (x,c) (e:es) 
  | x == e = (x,c+1) : countElems (x,c+1) es
  | x /= e = (x,c) : (e,1) : countElems (e,1) es
