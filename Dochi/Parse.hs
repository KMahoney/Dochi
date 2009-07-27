module Dochi.Parse ( AST(..)
                   , Interactive(..)
                   , ChiModuleAST(..)
                   , dochiParseLine
                   , dochiParseFile
                   , allExports
                   ) where

import Data.List (intersperse)
import Text.ParserCombinators.Parsec hiding (spaces)
import qualified Data.Map as M


-- |Syntax tree of a word definition

data AST = Word String
         | ModWord String String
         | CodeBlock [AST]
         | CallBlock AST
         | LString String
         | LChar Char
         | LInteger Integer
         | LKeyword String
         | BuildList [AST]
         | LList [AST]
         | LCons [AST]
         | LTable [AST]
         | Capture [String]
           deriving (Show)


-- |Interactive parse result

data Interactive = ILine [AST]
                 | IMod String
                 | IDef String [AST]
                 deriving (Show)


-- |Parse-tree representation of a module

data ChiModuleAST = ChiModuleAST { modName :: String
                                 , modDefs :: [(String, [AST])]
                                 , modUses :: [String]
                                 }


-- Parse monad

type ChiParser a = GenParser Char () a


instance Show ChiModuleAST where
    show m = "module " ++ (modName m) ++ " [" ++ concat (intersperse "," (map fst $ modDefs m)) ++ "]"

emptyModule = ChiModuleAST { modName = "", modDefs = [], modUses = [] }
addDef name ast m = m { modDefs = ((name, ast) : modDefs m) }


idletter = (alphaNum <|> oneOf "_-+?!/\\*<>=&;'%^£$~`¬|") <?> ""
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

litChar :: ChiParser AST
litChar = do string "Ch{"
             ch <- anyChar
             char '}'
             return $ LChar ch


lexCap :: ChiParser AST
lexCap = do char '('
            skipMany spaces
            v <- sepEndBy identifier (skipMany spaces)
            char ')'
            return $ Capture v


word :: ChiParser AST
word = do id <- identifier
          (char '.' >> identifier >>= (return . ModWord id)) <|> (return $ Word id)


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
                       , try litChar
                       , buildList
                       , litStr 
                       , callBlock
                       , keyword
                       , word
                       , codeQuot
                       , lexCap ] <?> "value"
           skipMany spaces
           return v



-- Begin word definition

defWord :: [ChiModuleAST] -> ChiParser [ChiModuleAST]
defWord (m:p) = do name <- identifier
                   skipMany1 spaces
                   values name []
    
    where values name l = choice [ toplevel $ addDef name (reverse l) m : p
                                 , value >>= values name . (:l)
                                 ]

defWord [] = unexpected "def outside of module"



-- Begin definition of a module

defModule :: [ChiModuleAST] -> ChiParser [ChiModuleAST]
defModule p = do name <- identifier
                 skipMany1 spaces
                 toplevel (emptyModule {modName = name} : p)



-- Declare modules to use with a list of identifiers

defUse :: [ChiModuleAST] -> [String] -> ChiParser [ChiModuleAST]
defUse (m:p) l = choice [ toplevel $ (m {modUses = l ++ (modUses m)}) : p
                        , do id <- identifier
                             skipMany1 spaces
                             defUse (m:p) (id:l)
                        ]

defUse [] _ = unexpected "use outside of module"



-- Parse interactive line

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


-- |Parse string as a standalone line

dochiParseLine :: String -> String -> Either ParseError Interactive
dochiParseLine name content = runParser interactive () name content



-- Top level of a file is either a module definition, a use list, or a word definition

toplevel :: [ChiModuleAST] -> ChiParser [ChiModuleAST]
toplevel p = skipMany spaces >>
             choice [ try (string "module" >> skipMany1 spaces) >> defModule p
                    , try (string "def" >> skipMany1 spaces) >> defWord p
                    , try (string "use" >> skipMany1 spaces) >> defUse p []
                    , eof >> return p
                    ]

file :: ChiParser [ChiModuleAST]
file = toplevel []


-- |Parse string as a file

dochiParseFile :: String -> String -> Either ParseError [ChiModuleAST]
dochiParseFile name content = runParser file () name content



-- |Return all words in a list of modules mapped to their module name

allExports :: [ChiModuleAST] -> M.Map String String
allExports = M.fromList . concatMap (\m -> map (\d -> (fst d, modName m)) (modDefs m))
