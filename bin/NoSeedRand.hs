module NoSeedRand ( uniformRands, normalRands ) where

import Data.Char
import qualified Data.ByteString.Lazy.Char8 as BS (drop,unpack,ByteString)

{--
main :: IO () 
main = do rs <- BS.readFile "/dev/random"
          putStr $ unlines $ map show (take 10 $ uniformRands rs)
--}

-- 0以上1未満の乱数列発生関数 --
uniformRands :: BS.ByteString -> [Double]
uniformRands bs = d : uniformRands (BS.drop 3 bs)
    where f (a:b:c:bs) = (ord a) * 256 * 256 + (ord b) * 256 + (ord c)
          n = f (BS.unpack bs)
          d = (fromIntegral n :: Double) / (256*256*256)

normalRands :: BS.ByteString -> [Double]
normalRands bs = f (uniformRands bs)
    where f rs = (sum $ take 12 rs) - 6.0 : f (drop 12 rs)
