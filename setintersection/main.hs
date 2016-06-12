import System.Environment (getArgs)
import Data.List

main :: IO ()
main = putStr . unlines . map f . lines =<< readFile . head =<< getArgs

f :: String -> String
f = intercalate "," . g . map (split (==',')) . split (==';')
  where
    g :: [[String]] -> [String]
    g [s1,s2] = s1 `intersect` s2
    g _       = []

split :: (a -> Bool) -> [a] -> [[a]]
split _ [] = [[]]
split p (x:xs)
  | p x       = []:split p xs
  | otherwise = case split p xs of
    []     -> [[x]]
    (y:ys) -> (x:y):ys
