module Dochi.Parse where

import Data.List (intersperse)
import Text.ParserCombinators.Parsec hiding (spaces)
import qualified Data.Map as M


data AST = Word String
         | CodeBlock [AST]
         | CallBlock AST
         | LString String
         | LInteger Integer
         | LKeyword String
         | BuildList [AST]
         | LList [AST]
         | LCons [AST]
         | LTable [AST]
         | Capture [String]
           deriving (Show)

data Interactive = ILine [AST]
                 | IMod String
                 | IDef String [AST]
                 deriving (Show)

data ChiModuleAST = ChiModuleAST { modName :: String
                                 , modDefs :: [(String, [AST])]
                                 , imports :: [String]
                                 , exports :: [String]
                                 }

type ChiParser a = GenParser Char () a


instance Show ChiModuleAST where
    show m = "module " ++ (modName m) ++ " [" ++ concat (intersperse "," (map fst $ modDefs m)) ++ "]"

emptyModule = ChiModuleAST { modName = "", modDefs = [], imports = [], exports = [] }
addDef name ast m = m { modDefs = ((name, ast) : modDefs m) }


idletter = (alphaNum <|> oneOf "_-+?!/\\*<>=.&;'%^£$~`¬|") <?> ""
identifier = (many1 idletter) <?> "identifier"


spaces = (comment <|> skipMany1 space) <?> ""
    where comment = do char '#'
                       manyTill anyChar newline
                       return ()


litInt :: ChiParser AST
litInt = do i <- many1 digit
            notFollowedBy idletter
            return $ LInteger $ read i


litStr :: ChiParser AST
litStr = (char '"') >> (manyTill anyChar $ char '"') >>= (return . LString)


lexCap :: ChiParser AST
lexCap = do char '('
            skipMany spaces
            v <- sepEndBy identifier (skipMany spaces)
            char ')'
            return $ Capture v


word :: ChiParser AST
word = identifier >>= (return . Word)


keyword :: ChiParser AST
keyword = char ':' >> (identifier <?> "word") >>= (return . LKeyword)


codeQuot :: ChiParser AST
codeQuot = do char '['
              skipMany spaces
              v <- manyTill value $ char ']'
              return $ CodeBlock v

buildList :: ChiParser AST
buildList = do char '{'
               skipMany spaces
               v <- manyTill value $ char '}'
               return $ BuildList v

litList :: ChiParser AST
litList = do char 'L'
             char '{'
             skipMany spaces
             v <- manyTill value $ char '}'
             return $ LList v

litCons :: ChiParser AST
litCons = do char 'C'
             char '{'
             skipMany spaces
             v <- manyTill value $ char '}'
             return $ LCons v

litTable :: ChiParser AST
litTable = do char 'T'
              char '{'
              skipMany spaces
              v <- manyTill value $ char '}'
              return $ LTable v


callBlock :: ChiParser AST
callBlock = do char '@'
               skipMany spaces
               v <- value
               return $ CallBlock v

value :: ChiParser AST
value = do v <- choice [ try litInt 
                       , try litList
                       , try litCons
                       , try litTable
                       , buildList
                       , litStr 
                       , callBlock
                       , keyword
                       , word
                       , codeQuot
                       , lexCap ] <?> "value"
           skipMany spaces
           return v

defWord :: [ChiModuleAST] -> ChiParser [ChiModuleAST]
defWord (m:p) = do name <- identifier
                   skipMany1 spaces
                   values name []
    
    where values name l = choice [ toplevel $ addDef name (reverse l) m : p
                                 , value >>= values name . (:l)
                                 ]

defWord [] = unexpected "def outside of module"


defModule :: [ChiModuleAST] -> ChiParser [ChiModuleAST]
defModule p = do name <- identifier
                 skipMany1 spaces
                 toplevel (emptyModule {modName = name} : p)


-- interactive

interactive :: ChiParser Interactive
interactive = iDef <|> iMod <|> iLine
    where iDef = do try (string "def" >> skipMany1 spaces)
                    name <- identifier
                    skipMany1 spaces
                    v <- manyTill value eof
                    return $ IDef name v

          iMod = do try (string "module" >> skipMany1 spaces)
                    name <- identifier
                    skipMany spaces
                    eof
                    return $ IMod name

          iLine = do skipMany spaces 
                     v <- manyTill value eof
                     return $ ILine v

dochiParseLine :: String -> String -> Either ParseError Interactive
dochiParseLine name content = runParser interactive () name content

-- files

toplevel :: [ChiModuleAST] -> ChiParser [ChiModuleAST]
toplevel p = skipMany spaces >>
             choice [ try (string "module" >> skipMany1 spaces) >> defModule p
                    , try (string "def" >> skipMany1 spaces) >> defWord p
                    , eof >> return p
                    ]

file :: ChiParser [ChiModuleAST]
file = toplevel []

dochiParseFile :: String -> String -> Either ParseError [ChiModuleAST]
dochiParseFile name content = runParser file () name content


-- exports

allExports :: [ChiModuleAST] -> M.Map String String
allExports = M.fromList . concatMap (\m -> map (\d -> (fst d, modName m)) (modDefs m))
