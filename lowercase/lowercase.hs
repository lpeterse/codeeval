import System.Environment (getArgs)
import Data.Char

main :: IO ()
main = putStr . map toUpper =<< readFile . head =<< getArgs
