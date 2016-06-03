import System.Environment

main :: IO ()
main  = putStr . unlines . map (show . f 0 . words) . lines =<< readFile . head =<< getArgs

f :: Int -> [String] -> Int
f i []                 = i
f i ("negative":xs)    = negate (f i xs)
f i ("zero":xs)        = f (i + 0) xs
f i ("one":xs)         = f (i + 1) xs
f i ("two":xs)         = f (i + 2) xs
f i ("three":xs)       = f (i + 3) xs
f i ("four":xs)        = f (i + 4) xs
f i ("five":xs)        = f (i + 5) xs
f i ("six":xs)         = f (i + 6) xs
f i ("seven":xs)       = f (i + 7) xs
f i ("eight":xs)       = f (i + 8) xs
f i ("nine":xs)        = f (i + 9) xs
f i ("ten":xs)         = f (i + 10) xs
f i ("eleven":xs)      = f (i + 11) xs
f i ("twelve":xs)      = f (i + 12) xs
f i ("thirteen":xs)    = f (i + 13) xs
f i ("fourteen":xs)    = f (i + 14) xs
f i ("fifteen":xs)     = f (i + 15) xs
f i ("sixteen":xs)     = f (i + 16) xs
f i ("seventeen":xs)   = f (i + 17) xs
f i ("eighteen":xs)    = f (i + 18) xs
f i ("nineteen":xs)    = f (i + 19) xs
f i ("twenty":xs)      = f (i + 20) xs
f i ("thirty":xs)      = f (i + 30) xs
f i ("forty":xs)       = f (i + 40) xs
f i ("fifty":xs)       = f (i + 50) xs
f i ("sixty":xs)       = f (i + 60) xs
f i ("seventy":xs)     = f (i + 70) xs
f i ("eighty":xs)      = f (i + 80) xs
f i ("ninety":xs)      = f (i + 90) xs
f i ("hundred":xs)     = f (i * 100) xs
f i ("thousand":xs)    = i * 1000 + f 0 xs
f i ("million":xs)     = i * 1000000 + f 0 xs
f i xs                 = error $ show i ++ " " ++ show xs
