import System.Environment (getArgs)

main :: IO ()
main = putStr . unlines . map (unwords . map f . words) . lines =<< readFile . head =<< getArgs

f :: String -> String
f ss = last ss : init (tail ss) ++ [head ss]
