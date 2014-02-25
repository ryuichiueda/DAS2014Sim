import System.Environment
import System.IO
import NoSeedRand
import qualified Data.ByteString.Lazy.Char8 as BS (readFile,ByteString)

type Distance = Double
type AngleDeg = Double

main :: IO () 
main = do rands <- BS.readFile "/dev/random"
          putStr . unlines $ map show (uniformRands rands)
