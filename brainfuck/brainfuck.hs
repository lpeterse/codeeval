import Control.Monad

main :: IO ()
main  = brainfuck helloWorld

type Tape = (String, Char, String)
type Prog = String

nil :: Tape
nil = let n = '\NUL' in (repeat n, n, repeat n)

mov :: Int -> Tape -> Tape
mov i (l,m,r) | i > 0     = mov (pred i) (m:l, head r, tail r)
              | i < 0     = mov (succ i) (tail l, head l, m:r)
              | otherwise = (l,m,r)

app :: (Char -> Char) -> Tape -> Tape
app f (l,m,r) = (l,f m,r)

get :: Tape -> Char
get (_,m,_) = m

brainfuck :: String -> IO ()
brainfuck s = void $ intr (0,nil,s)

intr :: (Int, Tape, Prog) -> IO (Int, Tape, Prog)
intr (off, tape, [])   = pure (off, tape, [])
intr (off, tape, x:xs) = case x of
  '>' -> intr (off + 1, mov 1 tape, xs)
  '<' -> intr (off - 1, mov (-1) tape, xs)
  '+' -> intr (off,  app succ tape, xs)
  '-' -> intr (off,  app pred tape, xs)
  '.' -> putChar (get tape) >> intr (off, tape, xs)
  ',' -> getChar >>= \c-> intr (off, app (const c) tape, xs)
  '[' -> loop (off, tape, xs) >>= intr
  ']' -> pure (off, tape, xs)
  _   -> intr (off, tape, xs)
  where
    loop (o,tp,ys) = do
      (o',tp',ys') <- intr (o,tp,ys)
      if get tp' /= '\NUL'
        then loop (o', tp', ys )
        else pure (o', tp', ys')

helloWorld :: String
--helloWorld = ""
--helloWorld = "+[--->++<]>+++.[->+++++++<]>.[--->+<]>----.++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+."
helloWorld  = "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.+++."
