module Parse(parseAST) where

import           AST
import           Text.Parsec
import           Text.Parsec.String (parseFromFile)

parseAST :: String -> IO (Either ParseError AST)
parseAST = parseFromFile file

file = do
    result <- many line
    eof
    return result

line = do
    n <- many1 digit
    many1 $ char ' '
    s <- statement
    many $ char ' '
    char ';'
    many $ char ' '
    char '\n'
    return $ Line (read n) s

statement = (try statementAgain) <|> (try statementDefer) <|> (try statementComma) <|> (try statementPrint) <|> statementDoExpr

statementAgain = do
    string "again"
    many $ char ' '
    char '('
    c <- condition
    char ')'
    many $ char ' '
    s <- statement
    return $ Again c s

statementDefer = do
    string "defer"
    many $ char ' '
    char '('
    c <- condition
    char ')'
    many $ char ' '
    s <- statement
    return $ Defer c s

statementComma = do
    d <- doExp
    many $ char ' '
    char ','
    many $ char ' '
    s <- statement
    return $ Comma d s

statementPrint = do
    string "print("
    many $ char ' '
    s <- strs
    many $ char ' '
    char ')'
    return $ Print s

statementDoExpr = do
    d <- doExp
    return $ Statement d

condition = (try conditionOr) <|> (try conditionAnd) <|> (try conditionNot) <|> conditionExp

conditionOr = do
    e <- expression
    many $ char ' '
    string "||"
    many $ char ' '
    c <- condition
    return $ Or e c

conditionAnd = do
    e <- expression
    many $ char ' '
    string "&&"
    many $ char ' '
    c <- condition
    return $ And e c

conditionNot = do
    char '!'
    many $ char ' '
    e <- expression
    return $ Not e

conditionExp = do
    e <- expression
    return $ Condition e

expression = (try $ exp2 AST.GT ">")
         <|> (try $ exp2 AST.LT "<")
         <|> (try $ exp2 AST.GE ">=")
         <|> (try $ exp2 AST.LE "<=")
         <|> (try $ exp2 AST.NE "!=")
         <|> (try $ exp2 AST.EQ "==")
         <|> expLExp 

exp2 f s = do
    l1 <- lExp
    many $ char ' '
    string s
    many $ char ' '
    l2 <- lExp
    return $ f l1 l2

expLExp = do
    l <- lExp
    return $ Exp l

doExp = (try doExpHash) <|> doExpLExp 

doExpHash = do
    f <- factor
    many $ char ' '
    char '#'
    many $ char ' '
    l <- lExp
    return $ Hash f l

doExpLExp = do
    l <- lExp
    return $ DoExp l

lExp = (try lExpAdd) <|> (try lExpSub) <|> lExpTerm

lExpAdd = do
    t1 <- term
    many $ char ' '
    char '+'
    many $ char ' '
    t2 <- term
    return $ Add t1 t2

lExpSub = do
    t1 <- term
    many $ char ' '
    char '-'
    many $ char ' '
    t2 <- term
    return $ Sub t1 t2

lExpTerm = do
    t <- term
    return $ LExp t

term = (try termMul) <|> (try termDiv) <|> (try termMod) <|> termFact

termMul = do
    f1 <- factor
    many $ char ' '
    char '*'
    many $ char ' '
    f2 <- factor
    return $ Mul f1 f2

termDiv = do
    f1 <- factor
    many $ char ' '
    char '/'
    many $ char ' '
    f2 <- factor
    return $ Div f1 f2

termMod = do
    f1 <- factor
    many $ char ' '
    char '%'
    many $ char ' '
    f2 <- factor
    return $ Mod f1 f2

termFact = do
    f <- factor
    return $ Term f

factor = (try factorPar) <|> (try factorRead) <|> (try factorNeg) <|> (try factorFunc) <|> factorInt

factorPar = do
    char '('
    many $ char ' '
    l <- lExp
    many $ char ' '
    char ')'
    return $ Par l

factorRead = do
    string "read()"
    return Read

factorNeg = do
    char '-'
    many $ char ' '
    l <- lExp
    return $ Neg l

factorFunc = (try $ funcFact N "N") <|> funcFact U "U"

funcFact f n = do
    string $ n ++ "("
    many $ char ' '
    l <- lExp
    many $ char ' '
    char ')'
    return $ FuncFact f l

factorInt = do
    n <- many1 digit
    return $ Factor $ read n

strs = (try strConcat) <|> strStrs

strConcat = do
    s <- str
    many $ char ' '
    char '+'
    many $ char ' '
    ss <- strs
    return $ Concat s ss

strStrs = do
    s <- str
    return $ Strs s

str = (try strString) <|> (try $ funcStr U "U") <|> strLExp

escape = do
    d <- char '\\'
    c <- oneOf "\\\"0nrvtbf" -- all the characters which can be escaped
    return [d, c]

nonEscape = noneOf "\\\"\0\n\r\v\t\b\f"

character = fmap return nonEscape <|> escape

strString = do
    char '\"'
    cs <- many character
    char '\"'
    return $ Str $ concat cs

funcStr f n = do
    string $ n ++ "("
    many $ char ' '
    l <- lExp
    many $ char ' '
    char ')'
    return $ FuncStr f l

strLExp = do
    l <- lExp
    return $ Num l