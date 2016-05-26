import System.Environment (getArgs)

import Data.List
import Data.Function

main :: IO ()
main = do
  file <- readFile . head =<< getArgs
  let x:xs = lines file
  putStr $ unlines $ take (read x) $ sortBy (flip compare `on` length) xs
