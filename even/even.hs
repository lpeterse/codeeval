import System.Environment
import Data.List
import Data.Char

main :: IO ()
main  = putStr . unlines . map foo . lines =<< readFile . head =<< getArgs

foo s = let (i,p) = iter 0 (read s) in show i ++ " " ++ show p

iter :: Int -> Integer -> (Int, Integer)
iter i p | palindrome (show p) = (i, p)
         | otherwise           = iter (succ i) (p + (read $ reverse $ show p))

palindrome x = x == reverse x
