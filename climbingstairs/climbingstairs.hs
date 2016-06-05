import System.Environment

main :: IO ()
main  = putStr . unlines . map f . lines =<< readFile . head =<< getArgs

f :: String -> String
f  = show . g . read

g :: Int -> Integer
g 0 = 0
g i = fibs !! (i+1)

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
