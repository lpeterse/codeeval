import System.Environment

main :: IO ()
main  = putStr . unlines . filter (not . null) . map (mth . reverse . words) . lines =<< readFile . head =<< getArgs

mth :: [String] -> String
mth []     = ""
mth (x:xs) | i < length xs = xs !! (read x - 1)
           | otherwise = ""
  where
    i = read x - 1
