import System.Environment (getArgs)

main :: IO ()
main = putStr . unlines . map f . lines =<< readFile . head =<< getArgs

f :: String -> String
f = show . (read :: String -> Int) . ('0':) . ('x':)
