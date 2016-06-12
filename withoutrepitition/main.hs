import System.Environment (getArgs)
import Data.List

main :: IO ()
main = putStr . map head . group =<< readFile . head =<< getArgs
