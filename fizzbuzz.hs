import System.Environment (getArgs)

main :: IO ()
main = mapM_ f . lines =<< readFile . head =<< getArgs
  where
    f s = case map read (words s) of
      [a,b,c] -> putStrLn (g a b (c :: Int))
      _       -> pure ()
    g x y z = unwords $ map t [1..z]
      where
        t i | i `mod` x == 0 && i `mod` y == 0 = "FB"
            | i `mod` x == 0 = "F"
            | i `mod` y == 0 = "B"
            | otherwise      = show i
