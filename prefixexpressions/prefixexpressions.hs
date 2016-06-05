import System.Environment
import Data.Ratio

main :: IO ()
main  = putStr . unlines . map f . lines =<< readFile . head =<< getArgs

f :: String -> String
f  = show . numerator . fst . eval . words

eval :: [String] -> (Rational, [String])
eval []       = (0, [])
eval (x:xs)   = case x of
                  "+" -> g (+)
                  "*" -> g (*)
                  "/" -> g (/)
                  _   -> ((read x :: Integer) % 1, xs)
  where
    g h = let (a,xs') = eval xs in let (b,xs'') = eval xs' in (h a b,xs'')
