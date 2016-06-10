import System.Environment (getArgs)
import Data.Bits

main :: IO ()
main = putStr . unlines . map (show . (popCount :: Integer -> Int) . read) . lines =<< readFile . head =<< getArgs
