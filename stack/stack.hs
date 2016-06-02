import System.Environment (getArgs)

main :: IO ()
main = putStr . unlines . map (unwords . f . reverse . words) . lines =<< readFile . head =<< getArgs

f :: [String] -> [String]
f [] = []
f [x] = [x]
f (x:_:zs) = x:f zs
