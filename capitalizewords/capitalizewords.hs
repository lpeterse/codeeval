import System.Environment (getArgs)
import Data.Char

main :: IO ()
main = putStr . unlines . map (unwords . map cap . words) . lines =<< readFile . head =<< getArgs

cap :: String -> String
cap [] = []
cap (x:xs) = toUpper x:xs
