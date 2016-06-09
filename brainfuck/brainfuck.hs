type Input = String
type State = String

main :: IO ()
main  = brainfuck helloWorld

brainfuck :: String -> IO ()
brainfuck s = i s ([], repeat '\NUL') >> pure ()

i :: Input -> (State, State) -> IO (State, State)
i []     st         = pure st
i (x:xs) st@(ls,rs) = case x of
  '>' -> i xs (head rs : ls, tail rs)
  '<' -> i xs (tail ls, head ls : rs)
  '+' -> i xs (ls, succ (head rs) : tail rs)
  '-' -> i xs (ls, pred (head rs) : tail rs)
  '.' -> putChar (head rs) >> i xs st
  ',' -> getChar >>= \c-> i xs (ls, c : tail rs)
  '[' -> loop st (takeWhile (/= ']') xs) >>= i (tail $ dropWhile (/= ']') xs)
  _   -> i xs st
  where
    loop tt ys = i xs tt >>= \tt'-> if head (snd tt') == '\NUL' then pure tt' else loop tt' ys

helloWorld :: String
helloWorld  = "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.+++."
