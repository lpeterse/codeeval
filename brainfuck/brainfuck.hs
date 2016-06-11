import Data.Word
import System.Environment (getArgs)

main :: IO ()
main = mapM_ brainfuck =<< pure . lines =<< readFile . head =<< getArgs

brainfuck :: String -> IO ()
brainfuck s = intr (repeat n,n,repeat n,s) >> putChar '\n'
  where
    n                 = 0 :: Word8
    intr (l,m,r,[])   = pure (l,m,r,[])
    intr (l,m,r,x:xs) = case x of
      '>' -> intr (m:l,head r,tail r,xs)
      '<' -> intr (tail l,head l,m:r,xs)
      '+' -> intr (l,m + 1,r,xs)
      '-' -> intr (l,m - 1,r,xs)
      '.' -> putChar (toEnum $ fromIntegral m) >> intr (l,m,r,xs)
      ',' -> getChar >>= \c-> intr (l,fromIntegral $ fromEnum c,r,xs)
      '[' -> loop (l,m,r,xs)
      ']' -> pure (l,m,r,xs)
      _   -> intr (l,m,r,xs)
    loop (a,b,c,d) = intr (a,b,c,d) >>= \(e,f,g,h)->
      if f /= n then loop (e,f,g,d) else intr (e,f,g,h)
