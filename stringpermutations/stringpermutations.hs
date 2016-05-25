import Data.List
import System.Environment

main :: IO ()
main = putStr . unlines . map f . lines =<< readFile . head =<< getArgs
  where
    f = intercalate "," . sort . permutations
