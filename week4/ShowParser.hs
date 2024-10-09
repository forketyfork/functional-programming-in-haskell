module ShowParser ( parseShow ) where
import Text.ParserCombinators.Parsec -- parser combinators
import qualified Text.ParserCombinators.Parsec.Token as P -- different types of tokens
import Text.ParserCombinators.Parsec.Language -- basics of programming languages

lexer = P.makeTokenParser emptyDef

parens        = P.parens lexer
brackets      = P.brackets lexer
braces        = P.braces lexer
commaSep      = P.commaSep lexer
whiteSpace    = P.whiteSpace lexer
symbol        = P.symbol lexer
identifier    = P.identifier lexer
integer       = P.integer lexer
stringLiteral = P.stringLiteral lexer

xmlHeader = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"

parseShow :: String -> String
parseShow str = xmlHeader ++ myRunParser showParser str

myRunParser :: Parser a -> String -> a
myRunParser p str = case parse p "" str of
                     Left err  -> error $ "parse error at " ++ show err
                     Right val -> val

otag t = "<" ++ t ++ ">"
ctag t = "</" ++ t ++ ">"
tag t v = concat [otag t, v, ctag t]
tagAttrs t attrs v = concat [otag (unwords $ t : map (\(k, v) -> concat [k, "=\"", v, "\""]) attrs), v, ctag t]

showParser :: Parser String
showParser =
    listParser <|> -- [ ... ]
    tupleParser <|> -- ( ... )
    try recordParser <|> -- MkRec { ... }
    adtParser <|> -- MkADT ...
    number <|> -- signed integer
    quotedString <?> "Parse error"

quotedString = do
    s <- stringLiteral
    return $ "\"" ++ s ++ "\""

number = do show <$> integer

listParser = do
    ls <- brackets $ commaSep showParser
    return $ tag "list" $ unlines $ map (tag "list-elt") ls

tupleParser = do
    ls <- parens $ commaSep showParser
    return $ tag "tuple" $ unwords $ map (tag "tuple-elt") ls

recordParser = do
    ti <- typeIdentifier
    ls <- braces $ commaSep kvparser
    return $ tagAttrs "record" [("name",ti)] (unlines ls)

adtParser = do tag "adt" <$> typeIdentifier

kvparser = do
    k <- identifier
    symbol "="
    tagAttrs "elt" [("key",k)] <$> showParser

typeIdentifier = do
    fst <- oneOf ['A'..'Z']
    rest <- many alphaNum
    whiteSpace
    return $ fst:rest
