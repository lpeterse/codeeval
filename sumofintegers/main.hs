import System.Environment (getArgs)
import Data.List

main :: IO ()
main = putStr . unlines . map f . lines =<< readFile . head =<< getArgs

f :: String -> String
f s = intercalate "," . map show $ g (read ("[" ++ s ++ "]") :: [Int])
  where
    g [] = []
    g (x:xs) = x:(g $ dropWhile (==x) xs)
