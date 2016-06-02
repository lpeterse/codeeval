import System.Environment

main :: IO ()
main  = putStr . unlines . map (show . f . reverse . words) . lines =<< readFile . head =<< getArgs

f :: [String] -> Int
f ("negative":xs)    = negate (f xs)
f ("zero":_)         = 0
f ("one":xs)         = g 1 xs
f ("two":xs)         = g 2 xs
f ("three":xs)       = g 3 xs
f ("four":xs)        = g 4 xs
f ("five":xs)        = g 5 xs
f ("six":xs)         = g 6 xs
f ("seven":xs)       = g 7 xs
f ("eight":xs)       = g 8 xs
f ("nine":xs)        = g 9 xs
f ("ten":xs)         = g 10 xs
f ("eleven":xs)      = g 11 xs
f ("twelve":xs)      = g 12 xs
f ("thirteen":xs)    = g 13 xs
f ("fourteen":xs)    = g 14 xs
f ("fifteen":xs)     = g 15 xs
f ("sixteen":xs)     = g 16 xs
f ("seventeen":xs)   = g 17 xs
f ("eighteen":xs)    = g 18 xs
f ("nineteen":xs)    = g 19 xs
f ("twenty":xs)      = g 20 xs
f ("thirty":xs)      = g 30 xs
f ("forty":xs)       = g 40 xs
f ("fifty":xs)       = g 50 xs
f ("sixty":xs)       = g 60 xs
f ("seventy":xs)     = g 70 xs
f ("eighty":xs)      = g 80 xs
f ("ninety":xs)      = g 90 xs
f xs                 = g 0  xs

g :: Int -> [String] -> Int
g i []               = i
g i ("hundred":xs)   = i + 100     * f xs
g i ("thousand":xs)  = i + 10000   * f xs
g i ("million":xs)   = i + 1000000 * f xs
g i xs               = i           + f xs
