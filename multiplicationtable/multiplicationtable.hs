
main :: IO ()
main = putStr $ unlines $ map f [1..12]

f :: Int -> String
f i = concatMap (leftpad . show) $ map (*i) [1..12]

leftpad :: String -> String
leftpad s = replicate (4 - length s) ' ' ++ s
