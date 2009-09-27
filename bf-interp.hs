import Control.Monad.State
import Data.Maybe (fromJust)
import Data.Char (ord, chr)
import Data.List (findIndex)
import Data.Word (Word8)
import System.Environment (getArgs)

data Status = Status {
  left :: String,
  right :: String,
  tape :: ([Word8], Int)
} deriving (Show, Eq)

main = do
  args <- getArgs
  if null args
    then getContents >>= runBrainfuck
    else mapM_ (\s -> readFile s >>= runBrainfuck) args
    
runBrainfuck :: String -> IO ()
runBrainfuck s = evalStateT vm Status { left = "", right = s, tape = (repeat 0, 0) }

vm :: StateT Status IO ()
vm = do
  status <- get
  unless (null (right status)) $ do
    let (insn:rest) = right status
    let (mem,at) = tape status
    
    case insn of
      '>' -> modify (\s -> s { tape = (mem, at+1) }) >> modify (move 1) >> vm
      '<' -> modify (\s -> s { tape = (mem, at-1) }) >> modify (move 1) >> vm
      '+' -> modify (\s -> s { tape = (inc at mem, at) }) >> modify (move 1) >> vm
      '-' -> modify (\s -> s { tape = (dec at mem, at) }) >> modify (move 1) >> vm
      '.' -> liftIO (putWord8 (mem !! at)) >> modify (move 1) >> vm
      ',' -> do
              c <- fmap (fromIntegral . ord) (liftIO getChar)
              modify (\s -> s { tape = (set c at mem, at) })
              modify (move 1)
              vm
      '[' -> if mem !! at == 0
              then modify jumpForward >> vm
              else modify (move 1) >> vm
      ']' -> modify jumpBackward >> vm
      _ -> modify (move 1) >> vm

putWord8 :: Word8 -> IO ()
putWord8 = putChar . chr . fromIntegral

move :: Int -> Status -> Status
move i s
  | i == 0 = s
  | i > 0 = let s' = move (i-1) s
                (x:xs) = right s'
            in s' { left = x:left s', right = xs }
  | i < 0 = let s' = move (i+1) s
                (x:xs) = left s'
            in s' { left = xs, right = x:right s' }

modifyAt :: (a -> a) -> Int -> [a] -> [a]
modifyAt f i ls = let (l,c:r) = splitAt i ls in l ++ f c:r

set :: a -> Int -> [a] -> [a]
set = modifyAt . const

inc :: Num a => Int -> [a] -> [a]
inc = modifyAt (+1)

dec :: Num a => Int -> [a] -> [a]
dec = modifyAt (subtract 1)

jumpForward :: Status -> Status
jumpForward s = go 0 (move 1 s)
  where
    go :: Int -> Status -> Status
    go (-1) s = s
    go n s = case head (right s) of
              '[' -> go (n+1) (move 1 s)
              ']' -> go (n-1) (move 1 s)
              _ -> go n (move 1 s)

jumpBackward :: Status -> Status
jumpBackward s = go 0 s
  where
    go :: Int -> Status -> Status
    go (-1) s = s
    go n s = case head (left s) of
              ']' -> go (n+1) (move (-1) s)
              '[' -> go (n-1) (move (-1) s)
              _ -> go n (move (-1) s)

