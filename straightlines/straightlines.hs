import System.Environment
import Data.Ratio
import Data.List

main :: IO ()
main  = putStr . unlines . map (show . t . map ((\[a,b]->(read a,read b)) . words). spill) . lines =<< readFile . head =<< getArgs

spill :: String -> [String]
spill [] = []
spill xs = let (a,b) = span (/= '|') xs in a:spill (drop 1 b)

t   :: [(Integer, Integer)] -> Integer
t []     = 0
t (x:xs) = t2 xs + t xs
  where
    t2 []     = 0
    t2 (y:ys) = foldl' q 0 ys + t2 ys
      where
        q i z | h fst x y z || h snd x y z || g x y z = succ i
              | otherwise                             = i

h k a b c = k a == k b && k b == k c
g (a1,a2) (b1,b2) (c1,c2) =
  dab1 /= 0 && dab2 /= 0 && dcb1 /= 0 && dcb2 /= 0 && dab == dcb
  where
    dab1 = a1 - b1
    dab2 = a2 - b2
    dcb1 = b1 - c1
    dcb2 = b2 - c2
    dab  = dab1 % dab2
    dcb  = dcb1 % dcb2
