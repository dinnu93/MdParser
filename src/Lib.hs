module Lib where

import Control.Monad (void)
import Control.Monad.Combinators
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

-- Types

type Document = [Block]

data Block = Heading Int 
           deriving (Show,Eq)

-- Parser

type Parser = Parsec Void String

