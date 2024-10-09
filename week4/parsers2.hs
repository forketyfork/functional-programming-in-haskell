import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr

lexer = P.makeTokenParser emptyDef

identifier = P.identifier lexer
reservedOp = P.reservedOp lexer

data Tag = MkTag String deriving Show

parseTag :: Parser Tag
parseTag = do
    char '<'
    x <- identifier
    char '>'
    return (MkTag x)

parseDiv :: Parser Tag
parseDiv = do
    string "<div>"
    return (MkTag "div")

letter_digit :: Parser Char
letter_digit = do
    x <- letter <|> digit
    return x

bag_bog :: Parser String
bag_bog = do
    xs <- try (string "bag") <|> string "bog"
    return xs

