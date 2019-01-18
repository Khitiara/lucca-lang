module Lucca.Parsing.Instruction where

import Text.Parsec
import Text.Parsec.String
import Lucca.Parsing.Literals
import Lucca.MachineTypes
import Lucca.Instruction
import Data.Functor

consumeLine :: Parser ()
consumeLine = void $ manyTill space endOfLine

registerParser :: Parser Register
registerParser = (string "g" *> (GEN <$> intLit')) <|> 
    (PC <$ string "pc") <|> (CMP <$ string "cmp") <|> (RET <$ string "ret")
    
instructionParser :: Parser Instruction
instructionParser = choice [try $ Nop <$ string "nop" <* consumeLine
                          , try $ (string "load" *> spaces *> (Load <$> registerParser)) <* consumeLine
                          , try $ (string "store" *> spaces *> (Store <$> registerParser)) <* consumeLine
                          , try $ (string "move" *> fail "aww") <* consumeLine
                          , try $ (string "push.i" *> spaces *>  intLit <&> Push) <* consumeLine
                          , try $ (string "push.f" *> spaces  *> floatLit <&> Push) <* consumeLine
                          , try $ string "push.s" *> spaces *> strLit <&> Push
                          , try $ Pop <$ string "pop" <* consumeLine 
                          , try $ Dup <$ string "dup" <* consumeLine
                          , try $ Swap <$ string "swap" <* consumeLine
                          , try $ Ret <$ string "ret" <* consumeLine
                          , try $ Blt <$> (string "blt" *> intLit') <* consumeLine
                          , try $ SysInt <$> (string "sysi" *> spaces *> strLit')
                          , try $ Addi <$ string "add.i" <* consumeLine
                          , try $ Subi <$ string "sub.i" <* consumeLine
                          , try $ Muli <$ string "mul.i" <* consumeLine
                          , try $ Divi <$ string "div.i" <* consumeLine
                          , try $ Addf <$ string "add.f" <* consumeLine
                          , try $ Subf <$ string "sub.f" <* consumeLine
                          , try $ Mulf <$ string "mul.f" <* consumeLine
                          , try $ Divf <$ string "div.f" <* consumeLine
                          , try $ Lshi <$ string "lshf" <* consumeLine
                          , try $ Rshi <$ string "rshf" <* consumeLine
                          , try $ Cmp <$ string "cmp" <* consumeLine]

program' :: Parser [Instruction]
program' = many1 instructionParser
