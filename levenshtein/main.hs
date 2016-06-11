import System.Environment (getArgs)

main :: IO ()
main = putStr . unlines . argl . lines =<< readFile . head =<< getArgs

argl :: [String] -> [String]
argl xs = map (show . peers (foldl add [] dict)) tests
  where
    (tests, _:dict) = span (/= "END OF INPUT") xs

peers :: [[String]] -> String -> Int
peers gs input = foldr ((+) . length) 0 $ fst $ divide input gs

friends :: String -> String -> Bool
friends []  [] = True
friends [] [_] = True
friends [_] [] = True
friends (x:xs) (y:ys) | x == y    = friends xs ys
                      | otherwise = xs == ys || x:xs == ys || xs == y:ys
friends _ _ = False

add :: [[String]] -> String -> [[String]]
add groups x | null friendlyGroups = [x]:groups
             | otherwise           = (x : concat friendlyGroups) : unfriendlyGroups
  where
    (friendlyGroups, unfriendlyGroups) = divide x groups

divide :: String -> [[String]] -> ([[String]], [[String]])
divide _ [] = ([], [])
divide x (group:groups)
  | any (friends x) group = (group:friendlyGroups, unfriendlyGroups)
  | otherwise             = (friendlyGroups, group:unfriendlyGroups)
  where
    (friendlyGroups, unfriendlyGroups) = divide x groups
