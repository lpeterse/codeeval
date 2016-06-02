import System.Environment (getArgs)
import Data.List

main :: IO ()
main = putStr . unlines . map (unwords . reverse . sort . words) . lines =<< readFile . head =<< getArgs
