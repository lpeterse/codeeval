import System.Environment (getArgs)
import Data.Char

main :: IO ()
main = putStr . unlines . map (rollerCoaster True) . lines =<< readFile . head =<< getArgs

rollerCoaster :: Bool -> String -> String
rollerCoaster _ [] = []
rollerCoaster upper (x:xs)
  | not (isLetter x) = x:rollerCoaster upper xs
  | upper            = toUpper x:rollerCoaster False xs
  | otherwise        = toLower x:rollerCoaster True xs
