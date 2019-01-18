module Lucca.Parsing.Literals where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Language
import qualified Text.Parsec.Token as Token

import Control.Monad
import Data.Functor ((<&>))

import Lucca.MachineTypes

strLit :: Parser DataType
strLit = S <$> strLit'
          
fixEscapes :: Char -> Char
fixEscapes 'n' = '\n'
fixEscapes 'r' = '\r'
fixEscapes 't' = '\t'
fixEscapes c = c
          
strLit' :: Parser String
strLit' = manyTill strChar endOfLine
              where escape = char '\\' >> noneOf "\n\r" <&> fixEscapes
                    strChar = noneOf "\\\n" <|> escape

intLit :: Parser DataType
intLit = N <$> intLit'

intLit' :: Parser Int
intLit' = read <$> ((string "-" <> many1 digit) <|> (many1 digit))

floatLit :: Parser DataType
floatLit = F <$> Token.float (Token.makeTokenParser emptyDef)
