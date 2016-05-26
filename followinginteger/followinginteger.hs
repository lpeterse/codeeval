import System.Environment (getArgs)
import Data.List

main :: IO ()
main = putStr . unlines . map f . lines =<< readFile . head =<< getArgs

f  :: String -> String
f [] = []
f s = case next (reverse s) of
  Just x  -> reverse x
  Nothing -> let sorted = sort s
                 xs = filter (=='0') sorted
                 y:ys = filter (/='0') sorted
             in (y:) $ ('0':) $ ys ++ xs

next :: String -> Maybe String
next (x:y:zs) | x > y     = Just (y:x:zs)
              | otherwise = (x:) <$> next (y:zs)
next _        = Nothing
