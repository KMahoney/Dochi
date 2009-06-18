module Parse (AST(..), Interactive(..), Prog, dochiParse, dochiParseFile) where

import Text.ParserCombinators.Parsec
import qualified Data.Map as M


data AST = Word String
         | CodeBlock [AST]
         | CallBlock AST
         | LString String
         | LInteger Integer
         | LKeyword String
         | LList [AST]
         | LTable [AST]
         | Capture [String]
           deriving (Show)

data Interactive = ILine [AST]
                 | IDef String [AST]
                 deriving (Show)

type Prog = [(String, [AST])]

type ChiParser a = GenParser Char () a



idletter = (alphaNum <|> oneOf "_-+?!/\\*<>=.&;'%^£$~`¬|") <?> ""
identifier = (many1 idletter) <?> "identifier"

skipSpaces :: ChiParser ()
skipSpaces = skipMany (comment <|> skipMany1 space)
    where comment = do char '#'
                       manyTill anyChar newline
                       return ()

litInt :: ChiParser AST
litInt = x <?> "integer"
    where x = do i <- many1 digit
                 notFollowedBy idletter
                 return $ LInteger $ read i


litStr :: ChiParser AST
litStr = x <?> "string"
    where x = (char '"') >> (manyTill anyChar $ char '"') >>= (return . LString)


lexCap :: ChiParser AST
lexCap = x <?> "capture"
    where x = do char '('
                 skipSpaces
                 v <- sepEndBy identifier skipSpaces
                 char ')'
                 return $ Capture v


word :: ChiParser AST
word = x <?> "word"
    where x = identifier >>= (return . Word)


keyword :: ChiParser AST
keyword = x <?> "keyword"
    where x = char ':' >> (identifier <?> "word") >>= (return . LKeyword)


codeQuot :: ChiParser AST
codeQuot = x <?> "quote"
    where x = do char '['
                 skipSpaces
                 v <- manyTill value $ char ']'
                 return $ CodeBlock v


litList :: ChiParser AST
litList = x <?> "list"
    where x = do char 'L'
                 char '{'
                 skipSpaces
                 v <- manyTill value $ char '}'
                 return $ LList v


litTable :: ChiParser AST
litTable = x <?> "map"
    where x = do char 'T'
                 char '{'
                 skipSpaces
                 v <- manyTill value $ char '}'
                 return $ LTable v


callBlock :: ChiParser AST
callBlock = x <?> "call operator"
    where x = do char '@'
                 skipSpaces
                 v <- value
                 return $ CallBlock v


value :: ChiParser AST
value = do v <- choice [ try litInt 
                       , try litList
                       , try litTable
                       , litStr 
                       , callBlock
                       , keyword
                       , word
                       , codeQuot
                       , lexCap ] <?> "value"
           skipSpaces
           return v

defWord :: ChiParser ()
defWord = do skipSpaces
             string "def"
             skipMany1 space

interactive :: ChiParser Interactive
interactive = iDef <|> iLine
    where iDef = do try defWord
                    name <- identifier
                    skipSpaces
                    v <- manyTill value eof
                    return $ IDef name v
          iLine = do skipSpaces 
                     v <- manyTill value eof
                     return $ ILine v

file :: ChiParser Prog
file = defWord >> rec

    where rec = (eof >> return []) <|> 
                do name <- identifier
                   skipSpaces
                   v <- manyTill value (eof <|> (try defWord))
                   r <- rec
                   return $ (name, v) : r

dochiParse :: String -> String -> Either ParseError Interactive
dochiParse name content = runParser interactive () name content

dochiParseFile :: String -> String -> Either ParseError Prog
dochiParseFile name content = runParser file () name content
