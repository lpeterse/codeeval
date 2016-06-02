import System.Environment

main :: IO ()
main  = putStr . unlines . map (show . (fibs !!) . read) . lines =<< readFile . head =<< getArgs
  where
    fibs :: [Int]
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
