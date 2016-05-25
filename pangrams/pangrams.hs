import System.Environment
import Data.List
import Data.Char

main :: IO ()
main  = putStr . unlines . map pangram . lines =<< readFile . head =<< getArgs

pangram :: String -> String
pangram s = case foldl (flip delete) ['a'..'z'] $ map toLower s of
  [] -> "NULL"
  xs -> xs
