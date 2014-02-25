import System.Environment
import System.IO
import StateTrans
import NoSeedRand
import qualified Data.ByteString.Lazy.Char8 as BS (readFile)

-- standard input:x y theta[deg]
-- args : an action (fw,cw,ccw,stay)

main :: IO () 
main = do as <- getArgs
          rands <- BS.readFile "/dev/random"
          getContents >>= printState . (move $ actionToDelta (head $ normalRands rands) (as !! 0)) . head . strToStates

