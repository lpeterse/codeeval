import System.Environment (getArgs)

import qualified Data.Set as S
import qualified Data.IntMap.Strict as IM

main :: IO ()
main = argl . zipWith (\i x->(i, length x, x)) [1..] . lines =<< readFile . head =<< getArgs

argl :: [(Int, Int, String)] -> IO ()
argl xs = mapM_ (print . flip peers groups) tests
  where
    (tests, _:dict) = span (\(_,_,s)-> s /= "END OF INPUT") xs
    groups          = foldr add IM.empty dict

peers :: (Int, Int, String) -> IM.IntMap (IM.IntMap (S.Set String)) -> Int
peers x groups = foldl (\c im-> c + foldl (\d is-> d + S.size is) 0 im) 0 $ map (\n-> IM.findWithDefault IM.empty n groups) $ findFriendlyGroupIds x groups

add :: (Int, Int, String) -> IM.IntMap (IM.IntMap (S.Set String)) -> IM.IntMap (IM.IntMap (S.Set String))
add x@(n,l,s) groups = case friendlyIds of
  []  -> IM.insert n (IM.singleton l (S.singleton s)) groups
  [m] -> IM.adjust (IM.insertWith S.union l (S.singleton s)) m groups
  _   -> IM.insert n friendlyGroup unfriendlyGroups
  where
    friendlyIds      = findFriendlyGroupIds x groups
    friendlyGroups   = map (\g-> IM.findWithDefault IM.empty g groups) friendlyIds
    friendlyGroup    = IM.unionsWith S.union (IM.singleton l (S.singleton s) : friendlyGroups)
    unfriendlyGroups = foldr IM.delete groups friendlyIds

findFriendlyGroupIds :: (Int, Int, String) -> IM.IntMap (IM.IntMap (S.Set String)) -> [Int]
findFriendlyGroupIds (_,l,s) = IM.foldlWithKey (\ids n g-> if friendsInGroup g then n:ids else ids) []
  where
    friendsInGroup g = anyEqualFriends || anySmallerFriends || anyLargerFriends
      where
        anyEqualFriends = case IM.lookup l g of
          Nothing -> False
          Just ss -> any (equalFriend s) ss
        anySmallerFriends = case IM.lookup (l - 1) g of
          Nothing -> False
          Just ss -> any (unequalFriend s) ss
        anyLargerFriends = case IM.lookup (l + 1) g of
          Nothing -> False
          Just ss -> any (`unequalFriend` s) ss
        equalFriend (x:xs) (y:ys) | x == y = equalFriend xs ys
          | otherwise = xs == ys
        equalFriend _ _           = True
        unequalFriend (x:xs) yys@(y:ys) | x == y    = unequalFriend xs ys
          | otherwise = xs == yys
        unequalFriend _ _               = True
