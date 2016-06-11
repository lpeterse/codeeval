import System.Environment (getArgs)

main :: IO ()
main = putStr . unlines . map (map f. split (==';')) . lines =<< readFile . head =<< getArgs

f :: String -> Char
f "zero" = '0'
f "one" = '1'
f "two" = '2'
f "three" = '3'
f "four" = '4'
f "five" = '5'
f "six" = '6'
f "seven" = '7'
f "eight" = '8'
f "nine" = '9'
f _ = ' '

split :: (a -> Bool) -> [a] -> [[a]]
split _ [] = [[]]
split p (x:xs)
  | p x       = []:split p xs
  | otherwise = case split p xs of
    []     -> [[x]]
    (y:ys) -> (x:y):ys
