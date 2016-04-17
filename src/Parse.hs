module Parse(parseAST) where

import           AST
import           Control.Monad      (void)
import           Text.Parsec        (ParseError (..), Parsec, char, digit,
                                     endOfLine, eof, many, many1, noneOf, oneOf,
                                     string, try, (<|>))
import           Text.Parsec.String (parseFromFile)

parseAST :: String -> IO (Either ParseError AST)
parseAST = parseFromFile file

file :: Parsec String u AST
file = do
    result <- many line
    eof
    return result

line :: Parsec String u Line
line = do
    n <- many1 digit
    many1 $ char ' '
    s <- statement
    many $ char ' '
    char ';'
    many $ char ' '
    void endOfLine <|> eof
    return $ Line (read n) s

statement :: Parsec String u Statement
statement = (try statementAgain) <|> (try statementDefer) <|> (try statementComma) <|> (try statementPrint) <|> statementDoExpr

statementAgain :: Parsec String u Statement
statementAgain = do
    string "again"
    many $ char ' '
    char '('
    c <- condition
    char ')'
    many $ char ' '
    s <- statement
    return $ Again c s

statementDefer :: Parsec String u Statement
statementDefer = do
    string "defer"
    many $ char ' '
    char '('
    c <- condition
    char ')'
    many $ char ' '
    s <- statement
    return $ Defer c s

statementComma :: Parsec String u Statement
statementComma = do
    d <- doExp
    many $ char ' '
    char ','
    many $ char ' '
    s <- statement
    return $ Comma d s

statementPrint :: Parsec String u Statement
statementPrint = do
    string "print("
    many $ char ' '
    s <- strs
    many $ char ' '
    char ')'
    return $ Print s

statementDoExpr :: Parsec String u Statement
statementDoExpr = do
    d <- doExp
    return $ Statement d

condition :: Parsec String u Condition
condition = (try conditionOr) <|> (try conditionAnd) <|> (try conditionNot) <|> conditionExp

conditionOr :: Parsec String u Condition
conditionOr = do
    e <- expression
    many $ char ' '
    string "||"
    many $ char ' '
    c <- condition
    return $ Or e c

conditionAnd :: Parsec String u Condition
conditionAnd = do
    e <- expression
    many $ char ' '
    string "&&"
    many $ char ' '
    c <- condition
    return $ And e c

conditionNot :: Parsec String u Condition
conditionNot = do
    char '!'
    many $ char ' '
    e <- expression
    return $ Not e

conditionExp :: Parsec String u Condition
conditionExp = do
    e <- expression
    return $ Condition e

expression :: Parsec String u Exp
expression = (try $ exp2 AST.GT ">")
         <|> (try $ exp2 AST.LT "<")
         <|> (try $ exp2 AST.GE ">=")
         <|> (try $ exp2 AST.LE "<=")
         <|> (try $ exp2 AST.NE "!=")
         <|> (try $ exp2 AST.EQ "==")
         <|> expLExp

exp2 :: (LExp -> LExp -> Exp) -> String -> Parsec String u Exp
exp2 f s = do
    l1 <- lExp
    many $ char ' '
    string s
    many $ char ' '
    l2 <- lExp
    return $ f l1 l2

expLExp :: Parsec String u Exp
expLExp = do
    l <- lExp
    return $ Exp l

doExp :: Parsec String u DoExp
doExp = (try doExpHash) <|> doExpLExp

doExpHash :: Parsec String u DoExp
doExpHash = do
    f <- factor
    many $ char ' '
    char '#'
    many $ char ' '
    l <- lExp
    return $ Hash f l

doExpLExp :: Parsec String u DoExp
doExpLExp = do
    l <- lExp
    return $ DoExp l

lExp :: Parsec String u LExp
lExp = (try lExpAdd) <|> (try lExpSub) <|> lExpTerm

lExpAdd :: Parsec String u LExp
lExpAdd = do
    t <- term
    many $ char ' '
    char '+'
    many $ char ' '
    l <- lExp
    return $ Add t l

lExpSub :: Parsec String u LExp
lExpSub = do
    t <- term
    many $ char ' '
    char '-'
    many $ char ' '
    l <- lExp
    return $ Sub t l

lExpTerm :: Parsec String u LExp
lExpTerm = do
    t <- term
    return $ LExp t

term :: Parsec String u Term
term = (try termMul) <|> (try termDiv) <|> (try termMod) <|> termFact

termMul :: Parsec String u Term
termMul = do
    f1 <- factor
    many $ char ' '
    char '*'
    many $ char ' '
    f2 <- factor
    return $ Mul f1 f2

termDiv :: Parsec String u Term
termDiv = do
    f1 <- factor
    many $ char ' '
    char '/'
    many $ char ' '
    f2 <- factor
    return $ Div f1 f2

termMod :: Parsec String u Term
termMod = do
    f1 <- factor
    many $ char ' '
    char '%'
    many $ char ' '
    f2 <- factor
    return $ Mod f1 f2

termFact :: Parsec String u Term
termFact = do
    f <- factor
    return $ Term f

factor :: Parsec String u Factor
factor = (try factorPar) <|> (try factorRead) <|> (try factorNeg) <|> (try factorFunc) <|> factorInt

factorPar :: Parsec String u Factor
factorPar = do
    char '('
    many $ char ' '
    l <- lExp
    many $ char ' '
    char ')'
    return $ Par l

factorRead :: Parsec String u Factor
factorRead = do
    string "read()"
    return Read

factorNeg :: Parsec String u Factor
factorNeg = do
    char '-'
    many $ char ' '
    l <- lExp
    return $ Neg l

factorFunc :: Parsec String u Factor
factorFunc = (try $ funcFact N "N") <|> funcFact U "U"

funcFact :: Function -> String -> Parsec String u Factor
funcFact f n = do
    string $ n ++ "("
    many $ char ' '
    l <- lExp
    many $ char ' '
    char ')'
    return $ FuncFact f l

factorInt :: Parsec String u Factor
factorInt = do
    n <- many1 digit
    return $ Factor $ read n

strs :: Parsec String u Strs
strs = (try strConcat) <|> strStrs

strConcat :: Parsec String u Strs
strConcat = do
    s <- str
    many $ char ' '
    char '+'
    many $ char ' '
    ss <- strs
    return $ Concat s ss

strStrs :: Parsec String u Strs
strStrs = do
    s <- str
    return $ Strs s

str :: Parsec String u Str
str = (try strString) <|> (try $ funcStr U "U") <|> strLExp

escape :: Parsec String u [Char]
escape = do
    d <- char '\\'
    c <- oneOf "\\\"0nrvtbf" -- all the characters which can be escaped
    return [d, c]

nonEscape :: Parsec String u Char
nonEscape = noneOf "\\\"\0\n\r\v\t\b\f"

character :: Parsec String u [Char]
character = fmap return nonEscape <|> escape

strString :: Parsec String u Str
strString = do
    char '\"'
    cs <- many character
    char '\"'
    return $ Str $ concat cs

funcStr :: Function -> String -> Parsec String u Str
funcStr f n = do
    string $ n ++ "("
    many $ char ' '
    l <- lExp
    many $ char ' '
    char ')'
    return $ FuncStr f l

strLExp :: Parsec String u Str
strLExp = do
    l <- lExp
    return $ Num l
