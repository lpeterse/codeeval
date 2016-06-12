import System.Environment (getArgs)
import Data.List

main :: IO ()
main = putStr . unlines . map f . lines =<< readFile . head =<< getArgs

f :: String -> String
f = show . g . split (==',')
  where
    g :: [String] -> Int
    g [s,[c]] = case elemIndices c s of
                  [] -> -1
                  xs -> last xs
    g _       = -1

split :: (a -> Bool) -> [a] -> [[a]]
split _ [] = [[]]
split p (x:xs)
  | p x       = []:split p xs
  | otherwise = case split p xs of
    []     -> [[x]]
    (y:ys) -> (x:y):ys
