import System.Environment (getArgs)
import qualified Data.Map as M
import qualified Data.Set as S

main :: IO ()
main = argl . map (\x->(length x, x)) . lines =<< readFile . head =<< getArgs

argl :: [(Int, String)] -> IO ()
argl xs = mapM_ (print . peers (foldl add [] dict)) tests
  where
    (tests, _:dict) = span (/= (12, "END OF INPUT")) xs

peers :: [M.Map Int (S.Set String)] -> (Int, String) -> Int
peers gs input = foldl (\i m-> i + foldl (\j s-> j + S.size s) 0 m) 0 $ fst $ divide input gs

add :: [M.Map Int (S.Set String)] -> (Int,String) -> [M.Map Int (S.Set String)]
add groups x@(i,s) | null friendlyGroups = singleGroup : groups
                   | otherwise           = friendlyGroup : unfriendlyGroups
  where
    (friendlyGroups, unfriendlyGroups) = divide x groups
    singleGroup   = M.singleton i (S.singleton s)
    friendlyGroup = M.unionsWith S.union (singleGroup : friendlyGroups)

divide :: (Int,String) -> [M.Map Int (S.Set String)] -> ([M.Map Int (S.Set String)], [M.Map Int (S.Set String)])
divide _ [] = ([], [])
divide t@(i,s) (group:groups)
  | friendsInGroup = (group:friendlyGroups, unfriendlyGroups)
  | otherwise      = (friendlyGroups, group:unfriendlyGroups)
  where
    (friendlyGroups, unfriendlyGroups) = divide t groups
    friendsInGroup = anyEqualFriends || anySmallerFriends || anyLargerFriends
    anyEqualFriends = case M.lookup i group of
      Nothing -> False
      Just ss -> any (equalFriend s) ss
    anySmallerFriends = case M.lookup (i - 1) group of
      Nothing -> False
      Just ss -> any (unequalFriend s) ss
    anyLargerFriends = case M.lookup (i + 1) group of
      Nothing -> False
      Just ss -> any (`unequalFriend` s) ss

    equalFriend (x:xs) (y:ys) | x == y = equalFriend xs ys
                              | otherwise = xs == ys
    equalFriend _ _           = True

    unequalFriend (x:xs) yys@(y:ys) | x == y    = unequalFriend xs ys
                                    | otherwise = xs == yys
    unequalFriend _ _               = True
