import System.Environment (getArgs)

main :: IO ()
main = print . sum . map (read :: String -> Integer) . lines =<< readFile . head =<< getArgs
