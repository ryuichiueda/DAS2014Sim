import System.Environment
import System.IO
import StateTrans

-- standard :: a record: input:x y theta[deg] weight

type Particle = [Double] -- x y theta w

main :: IO () 
main = getContents >>= main' . readParticles
        
readParticles :: String -> [Particle]
readParticles cs = [ map (\w -> read w :: Double) $ words ln | ln <- lines cs ]

main' :: [Particle] -> IO ()
main' ps = putStr $ unlines [ unwords $ map show $ weightChange p | p <- ps ]

weightChange :: Particle -> Particle
weightChange p@[x,y,t,w] = if inGoal (x,y,t) then [x,y,t,w*0.00001] else p

{--
= [x,y,t,w']
    where ell2 = (xg - x)*(xg - x) + (yg - y)*(yg - y)
          w' = if ell2 < r*r then 0.0 else w
--}
        
