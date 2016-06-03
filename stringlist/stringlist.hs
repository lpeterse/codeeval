import System.Environment
import Data.List

main :: IO ()
main  = putStr . unlines . map f . lines =<< readFile . head =<< getArgs

f :: String -> String
f xs = intercalate "," $ g (read i) (sort $ nub ys)
  where
    (i, ',':ys) = break (==',') xs

g :: Int -> String -> [String]
g i xs = map o [0..((b ^ i) - 1)]
  where
    b     = length xs
    o t   = map ((xs !!) . k t) $ reverse [0..pred i]
    k q p = mod (div q (b ^ p)) b
