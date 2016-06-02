import System.Environment (getArgs)
import System.IO

main :: IO ()
main = getArgs >>= flip openFile ReadMode . head >>= hFileSize >>= print
