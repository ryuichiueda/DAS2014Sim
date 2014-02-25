import System.Environment
import System.IO
import StateTrans
import NoSeedRand
import qualified Data.ByteString.Lazy.Char8 as BS (readFile,ByteString)

-- standard input:x y theta[deg]

main :: IO () 
main = do rands <- BS.readFile "/dev/random"
          getContents >>= putStrLn . main' rands
          
main' :: BS.ByteString -> String -> String
main' rs cs 
  | ans == Nothing = ""
  | otherwise      = unwords $ map show [ fst ok , snd ok ]
      where ans = sensing (normalRands rs) (landmarkPos $ getPos cs)
            f (Just a) = a
            ok = f ans

sensing :: [Double] -> PolerPos -> PolerPos
sensing _          Nothing          = Nothing
sensing (e1:e2:es) (Just (ell,phi)) = Just (obsell,obsphi)
    where obsell = ell*(1.0 + 0.1*e1)
          obsphi = phi + 10.0*e2

getPos :: String -> State
getPos cs = f [ read w :: Double | w <- words cs ]
    where f [a,b,c] = (a,b,c)
