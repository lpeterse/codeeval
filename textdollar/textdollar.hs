import System.Environment

main :: IO ()
main  = putStr . unlines . map (f . read) . lines =<< readFile . head =<< getArgs

f :: Int -> String
f 0 = "ZeroDollars"
f 1 = "OneDollar"
f i = g i ++ "Dollars"

g :: Int -> String
g 0 = ""
g 1 = "One"
g 2 = "Two"
g 3 = "Three"
g 4 = "Four"
g 5 = "Five"
g 6 = "Six"
g 7 = "Seven"
g 8 = "Eight"
g 9 = "Nine"
g 10 = "Ten"
g 11 = "Eleven"
g 12 = "Twelve"
g 13 = "Thirteen"
g 14 = "Fourteen"
g 15 = "Fifteen"
g 16 = "Sixteen"
g 17 = "Seventeen"
g 18 = "Eighteen"
g 19 = "Nineteen"
g x | x < 30      =                      "Twenty"   ++ g (rem x      10)
    | x < 40      =                      "Thirty"   ++ g (rem x      10)
    | x < 50      =                      "Forty"    ++ g (rem x      10)
    | x < 60      =                      "Fifty"    ++ g (rem x      10)
    | x < 70      =                      "Sixty"    ++ g (rem x      10)
    | x < 80      =                      "Seventy"  ++ g (rem x      10)
    | x < 90      =                      "Eighty"   ++ g (rem x      10)
    | x < 100     =                      "Ninety"   ++ g (rem x      10)
    | x < 1000    = g (div x     100) ++ "Hundred"  ++ g (rem x     100)
    | x < 1000000 = g (div x    1000) ++ "Thousand" ++ g (rem x    1000)
    | otherwise   = g (div x 1000000) ++ "Million"  ++ g (rem x 1000000)
