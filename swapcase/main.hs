import System.Environment (getArgs)
import Data.Char

main :: IO ()
main = putStr . map cap =<< readFile . head =<< getArgs

cap :: Char -> Char
cap x | isUpper x = toLower x
      | isLower x = toUpper x
      | otherwise = x
