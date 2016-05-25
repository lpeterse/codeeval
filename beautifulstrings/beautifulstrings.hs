import Data.Char
import Data.List
import System.Environment

main :: IO ()
main  = putStr . unlines . map (show . maximumBeauty) . lines =<< readFile . head =<< getArgs

maximumBeauty :: String -> Int
maximumBeauty =
  sum . zipWith (*) (reverse [1..26]) . sortBy (flip compare) .
  map length . group . sort . map toLower . filter isLetter
