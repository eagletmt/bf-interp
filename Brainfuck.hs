module Brainfuck (
  BFInsn(..),
  parseBrainfuck,
  runBrainfuck,
  ) where

import Control.Monad.State
import Data.Maybe (fromJust)
import Data.Char (ord, chr)
import Data.Word (Word8)
import Text.ParserCombinators.Parsec

data BFInsn = Next | Prev | Inc | Dec | In | Out | Begin Int | End Int deriving (Show, Eq)

data Status = Status {
  left :: [BFInsn],
  right :: [BFInsn],
  tape :: ([Word8], Int)
}

runBrainfuck :: [BFInsn] -> IO ()
runBrainfuck s = evalStateT vm Status { left = [], right = s, tape = (repeat 0, 0) }

vm :: StateT Status IO ()
vm = do
  status <- get
  unless (null (right status)) $ do
    let (insn:rest) = right status
    let (mem,at) = tape status
    
    case insn of
      Next -> modify (\s -> s { tape = (mem, at+1) }) >> modify (move 1) >> vm
      Prev -> modify (\s -> s { tape = (mem, at-1) }) >> modify (move 1) >> vm
      Inc -> modify (\s -> s { tape = (inc at mem, at) }) >> modify (move 1) >> vm
      Dec -> modify (\s -> s { tape = (dec at mem, at) }) >> modify (move 1) >> vm
      Out -> liftIO (putWord8 (mem !! at)) >> modify (move 1) >> vm
      In -> do
              c <- fmap (fromIntegral . ord) (liftIO getChar)
              modify (\s -> s { tape = (set c at mem, at) })
              modify (move 1)
              vm
      Begin i -> modify (if mem !! at == 0 then move i else move 1) >> vm
      End i -> modify (move (-i)) >> vm

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

parseBrainfuck :: SourceName -> String -> Either ParseError [BFInsn]
parseBrainfuck name = parse bf name . filter (`elem` "><+-,.[]")
  where
    bf :: Parser [BFInsn]
    bf = do
      insns <- expr
      eof
      return insns

    expr :: Parser [BFInsn]
    expr = do
      a <- many (oneOf "><+-,.")
      b <- try (do { char '['; es <- expr; char ']'; return es }) <|> return []
      c <- many (oneOf "><+-,.")
      return $ toBFInsn a
        ++ if null b then [] else [Begin (2 + length b)] ++ b ++ [End (1 + length b)]
        ++ toBFInsn c

    toBFInsn :: String -> [BFInsn]
    toBFInsn = map (fromJust . (`lookup` table))
    table :: [(Char, BFInsn)]
    table = [('>', Next), ('<', Prev), ('+', Inc), ('-', Dec), (',', In), ('.', Out)]

