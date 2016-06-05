import System.Environment

main :: IO ()
main  = putStr . unlines . map fasel . lines =<< readFile . head =<< getArgs

fasel :: String -> String
fasel  = show . length . filter ugly . concatMap (gnurp . map read) . partitions

ugly :: Int -> Bool
ugly 0 = True
ugly x = f 2 || f 3 || f 5 || f 7
  where
    f i = mod x i == 0

gnurp :: [Int] -> [Int]
gnurp [i]    = [i]
gnurp (i:is) = concatMap (\j->[i + j, i - j]) (gnurp is)

partitions :: String -> [[String]]
partitions []     = [[[]]]
partitions [c]    = [[[c]]]
partitions (c:cs) = concatMap (\(s:ss)-> [(c:s):ss, [c]:s:ss]) (partitions cs)
