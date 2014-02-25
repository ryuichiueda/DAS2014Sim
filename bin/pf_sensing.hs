import System.Environment
import System.IO
import StateTrans

-- standard :: a record: input:x y theta[deg] weight
-- args : observed distance, obaserved direction [deg] 

type Particle = [Double] -- x y theta w

main :: IO () 
main = do as <- getArgs
          getContents >>= main' (readObs as) . readParticles

readObs :: [String] -> PolerPos
readObs [] = Nothing
readObs (a:b:args) = Just (read a :: Double, read b :: Double)
        
readParticles :: String -> [Particle]
readParticles cs = [ map (\w -> read w :: Double) $ words ln | ln <- lines cs ]

main' :: PolerPos -> [Particle] -> IO ()
main' obs ps = putStr $ unlines [ unwords $ map show $ weightChange obs p | p <- ps ]

weightChange :: PolerPos -> Particle -> Particle
weightChange Nothing p = p
weightChange obs@(Just (ellobs,phiobs)) (x:y:t:w:_) 
  | lp == Nothing = [x,y,t,0.0]
  | otherwise     = [x,y,t,w*prob_ell*prob_phi]
    where lp = landmarkPos (x,y,t)
          ell = fst $ f lp
          phi = snd $ f lp
          prob_ell = gauss (ell*0.1) (ellobs - ell)
          prob_phi = gauss 10.0 (phiobs - phi)
          f (Just a) = a

gauss :: Double -> Double -> Double
gauss sigma diff = (exp h) / (sigma * (sqrt (2 * pi) ) ) 
    where h = - diff * diff / (2 * sigma * sigma) 
