import System.Environment (getArgs)

main :: IO ()
main = putStr . unlines . map (f . split (==',') . tail . dropWhile (/=';')) . lines =<< readFile . head =<< getArgs

split :: (a -> Bool) -> [a] -> [[a]]
split _ [] = [[]]
split p (x:xs)
  | p x       = []:split p xs
  | otherwise = case split p xs of
    []     -> [[x]]
    (y:ys) -> (x:y):ys

f :: [String] -> String
f [] = []
f (x:xs) | x `elem` xs = x
         | otherwise   = f xs
