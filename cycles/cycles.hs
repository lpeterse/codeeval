import System.Environment

main :: IO ()
main  = putStr . unlines . map (unwords . cycles . words) . lines =<< readFile . head =<< getArgs

cycles :: [String] -> [String]
cycles []
  = []
cycles (x:xs)
  | x `elem` xs = x:takeWhile (/=x) xs
  | otherwise   = cycles xs
