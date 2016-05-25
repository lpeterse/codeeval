import System.Environment (getArgs)
import Data.Bits

main :: IO ()
main = putStr . unlines . map f . lines =<< readFile . head =<< getArgs

f :: String -> String
f s = case map read (words s) of
  [a,b] -> show $ length $ filter (\x->64 - countLeadingZeros x - popCount x == a) [1..b]
  _     -> s
