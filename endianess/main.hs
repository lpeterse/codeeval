import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Ptr
import Data.Word

main :: IO ()
main = alloca $ \ptr-> do
  poke ptr (1 :: Word64)
  w8 <- peek (castPtr ptr)
  if w8 == (0 :: Word8)
    then putStrLn "BigEndian"
    else putStrLn "LittleEndian"
