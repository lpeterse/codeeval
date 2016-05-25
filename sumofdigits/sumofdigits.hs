import System.Environment

main :: IO ()
main  = putStr . unlines . map (show . sumofdigits) . lines =<< readFile . head =<< getArgs

sumofdigits :: String -> Int
sumofdigits = sum . map (read . pure)
