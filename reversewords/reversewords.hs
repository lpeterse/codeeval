import System.Environment (getArgs)

main :: IO ()
main = putStr . unlines . map (unwords . reverse . words) . lines =<< readFile . head =<< getArgs
