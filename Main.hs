import System.Environment (getArgs)
import Control.Monad (forM_)
import Brainfuck

main = do
  args <- getArgs
  if null args
    then getContents >>= either print runBrainfuck . parseBrainfuck "<stdin>"
    else forM_ args $ \s -> readFile s >>= either print runBrainfuck . parseBrainfuck s

