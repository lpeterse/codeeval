import System.Environment (getArgs)

main :: IO ()
main = putStr . unlines . map (show . fromBinary 0 . zeros . map length . words) . lines =<< readFile . head =<< getArgs

zeros :: [Int] -> [Bool]
zeros (1:x:xs) = replicate x False ++ zeros xs
zeros (2:x:xs) = replicate x True ++ zeros xs
zeros _ = []

fromBinary :: Integer -> [Bool] -> Integer
fromBinary i [] = i
fromBinary i (True:xs) = fromBinary (i*2 + 1) xs
fromBinary i (False:xs) = fromBinary (i*2) xs
