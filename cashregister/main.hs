import System.Environment (getArgs)
import Data.List

main :: IO ()
main = putStr . unlines . map cash . lines =<< readFile . head =<< getArgs

cash :: String -> String
cash xs = let (pp, _:ch) = span (/=';') xs in back (kok ch - kok pp)
  where
    kok i | '.' `elem` i = read $ filter (/='.') i
          | otherwise    = read i * 100
    back i | i  < 0    = "ERROR"
           | i == 0    = "ZERO"
           | otherwise = intercalate "," $ map glub (gnurp i)

gnurp :: Int -> [Int]
gnurp i = case dropWhile (i <) [10000, 5000, 2000, 1000, 500, 200, 100, 50, 25, 10, 5, 1] of
  []    -> []
  (x:_) -> x : gnurp (i - x)

glub :: Int -> String
glub 1 = "PENNY"
glub 5 = "NICKEL"
glub 10 = "DIME"
glub 25 = "QUARTER"
glub 50 = "HALF DOLLAR"
glub 100 = "ONE"
glub 200 = "TWO"
glub 500 = "FIVE"
glub 1000 = "TEN"
glub 2000 = "TWENTY"
glub 5000 = "FIFTY"
glub 10000 = "ONE HUNDRED"
glub _ = "ERROR"
