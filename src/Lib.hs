module Lib where

import Control.Monad 
import Control.Monad.Combinators
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Applicative ((<|>))

-- Types

data Document = Document [Block] deriving Show


-- Block level types

data Block = Heading Int [Inline]
           | Para [Inline]
           deriving (Show,Eq)

-- Inline types

data Inline = Text String
            | Emph [Inline]
            | Strong [Inline]
            | Break
            deriving (Show,Eq)
            
-- Parser

type Parser = Parsec Void String

parseDocument :: Parser Document
parseDocument = do
  ls <- some parseBlock
  return $ Document ls

parseBlock :: Parser Block
parseBlock = choice [try parseHeading, parseParagraph]

parseParagraph :: Parser Block
parseParagraph = do
  void (many eol)
  str <- some (anySingleBut '\n')
  choice [eof, void (string "\n\n")]
  return $ Para [Text str]

parseHeading :: Parser Block
parseHeading = do
  void (many eol)
  hashes <- some (char '#')
  guard (length hashes <= 6)
  void (some spaceChar)
  str <- some (anySingleBut '\n')
  choice [eof, void (char '\n')]
  void (many eol)
  return $ Heading (length hashes) [Text str]
