type Input = String
type State = String

main :: IO ()
main  = brainfuck helloWorld

brainfuck :: String -> IO ()
brainfuck s = i s (repeat '\NUL', '\NUL', repeat '\NUL') >> pure ()

i :: Input -> (State, Char, State) -> IO (State, Char, State)
i []     st         = pure st
i (x:xs) st@ (l,m,r) = case x of
  '>' -> i xs (m:l, head r, tail r)
  '<' -> i xs (tail l, head l, m:r)
  '+' -> i xs (l, succ m, r)
  '-' -> i xs (l, pred m, r)
  '.' -> putChar m >> i xs st
  ',' -> getChar >>= \c-> i xs (l,c,r)
  '[' -> i xs lmr
  _   -> i xs st
  where
    loop st'@(p', _, _) ys = do
      st''@(p'', _, _) <- i xs st'
      case compare p' p'' of
        EQ ->
      >>= \tt'-> if head (snd tt') == '\NUL' then pure tt' else loop tt' ys

helloWorld :: String
helloWorld  = "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.+++."
