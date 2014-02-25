import System.Environment
import System.IO
import StateTrans

-- standard :: pos input:x y theta[deg]

main :: IO () 
main = getContents >>= main' . readPos
        
readPos :: String -> [Double]
readPos cs = map (\w -> read w :: Double) (words cs)

main' :: [Double] -> IO ()
main' (x:y:t:_) = if inGoal (x,y,t) then putStr "OK" else putStr ""
