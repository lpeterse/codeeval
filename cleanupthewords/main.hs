import System.Environment (getArgs)
import Data.Char
import Data.List

main :: IO ()
main = putStr . unlines . map (intercalate " " . filter (not . null) . f) . lines =<< readFile . head =<< getArgs

f :: String -> [String]
f [] = []
f a = map toLower b : f e
  where
    (b,c) = span  isAlpha a
    (_,e) = break isAlpha c
