import System.Environment (getArgs)

main :: IO ()
main = putStr . unlines . map f . lines =<< readFile . head =<< getArgs

f :: String -> String
f s | length s <= 55 = s
    | otherwise      = (++ "... <Read More>") $ reverse $ g $ reverse (take 41 s)

g :: String -> String
g (' ': xs ) = dropWhile (== ' ') xs
g xs = dropWhile (== ' ') $ dropWhile (/= ' ') xs
