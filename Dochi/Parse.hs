module Parse (AST(..), Prog, dochiParse) where

import Text.ParserCombinators.Parsec
import qualified Data.Map as M


data AST = Word String
         | CodeBlock [AST]
         | LArray [AST]
         | LString String
         | LInteger Integer
         | Capture [String]
           deriving (Show)

type Prog = M.Map String AST

type ChiParser a = GenParser Char () a



idletter = (alphaNum <|> oneOf "_-+?!/\\*<>=.") <?> ""
identifier = (many1 idletter) <?> "identifier"

litInt :: ChiParser AST
litInt = i1 <?> "integer"
    where i1 = do i <- many1 digit
                  notFollowedBy idletter
                  return $ LInteger $ read i

litStr :: ChiParser AST
litStr = (s1 <?> "string")
    where s1 = (char '"') >> (manyTill anyChar $ char '"') >>= (return . LString)

lexCap :: ChiParser AST
lexCap = do char '|'
            spaces
            v <- sepEndBy identifier spaces
            char '|'
            return $ Capture v

word :: ChiParser AST
word = (identifier <?> "word") >>= (return . Word)

codeQuot :: ChiParser AST
codeQuot = do char '['
              spaces
              v <- manyTill value $ char ']'
              return $ CodeBlock v

litArr :: ChiParser AST
litArr = do char '{'
            spaces
            v <- manyTill value $ char '}'
            return $ LArray v

value :: ChiParser AST
value = do v <- (try litInt <|> (litStr <|> word <|> codeQuot <|> litArr <|> lexCap))
           spaces
           return v

dochiParse :: String -> String -> Either ParseError AST
dochiParse name content = runParser (spaces >> value) () name content
