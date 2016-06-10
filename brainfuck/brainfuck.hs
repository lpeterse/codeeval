type Input = String
type State = String

main :: IO ()
main  = brainfuck helloWorld

brainfuck :: String -> IO ()
brainfuck s = i s (repeat '\NUL', '\NUL', repeat '\NUL') >> pure ()

i :: (State, Char, State, Input) -> IO (State, Char, State, Input)
i (l,m,r, [])   = pure (l,m,r)
i (l,m,r, x:xs) = case x of
  '>' -> i xs (m:l, head r, tail r, xs)
  '<' -> i xs (tail l, head l, m:r, xs)
  '+' -> i xs (l, succ m, r, xs)
  '-' -> i xs (l, pred m, r, xs)
  '.' -> putChar m >> i (l, m, r, xs)
  ',' -> getChar >>= \c-> i (l, c, r, xs)
  '[' -> i xs lmr
  ']' -> pure (l, m, r, xs)
  _   -> i (l, m, r, xs)
  where
    loop st'@(p', _, _) ys = do
      st''@(p'', _, _) <- i xs st'
      case compare p' p'' of
        EQ ->
      >>= \tt'-> if head (snd tt') == '\NUL' then pure tt' else loop tt' ys

helloWorld :: String
helloWorld  = "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.+++."
