import System.Environment (getArgs)
import Numeric

main :: IO ()
main = putStr . unlines . map (\i-> showIntAtBase 2 (head . show) (read i :: Integer) "") . lines =<< readFile . head =<< getArgs
