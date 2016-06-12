import System.Environment (getArgs)

main :: IO ()
main = putStr . unlines . map f . lines =<< readFile . head =<< getArgs

f :: String -> String
f = show . g . map read .split (==',')
  where
    g :: [Int] -> Int
    g [a,b] = a - b * div a b
    g _     = 0

split :: (a -> Bool) -> [a] -> [[a]]
split _ [] = [[]]
split p (x:xs)
  | p x       = []:split p xs
  | otherwise = case split p xs of
    []     -> [[x]]
    (y:ys) -> (x:y):ys
