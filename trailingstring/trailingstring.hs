import System.Environment
import Data.List

main :: IO ()
main  = putStr . unlines . filter (not . null) . map trailing . lines =<< readFile . head =<< getArgs

trailing :: String -> String
trailing [] = ""
trailing s  =
  let (xs,_:ys) = break (== ',') s
  in if isSuffixOf ys xs then "1" else "0"
