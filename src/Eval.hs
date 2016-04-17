{-# LANGUAGE LambdaCase #-}

module Eval(evalAST) where

import           AST
import           Control.Monad (liftM, liftM2, filterM)
import           Data.Char     (isDigit, ord, chr, digitToInt)
import           Data.List     (delete)
import qualified Data.Map      as Map
import           Data.Random   (sample, shuffle)
import           System.IO     (hLookAhead, stdin, hGetChar)

type Program = Map.Map Int Statement
type Stack   = [Int]
type Keep    = Bool

evalAST :: AST -> IO ()
evalAST ast = eval program stack
    where
        program :: Program
        program = Map.fromList . map (\(Line n s) -> (n, s)) $ ast

        stack :: Stack
        stack = map (\(Line n _) -> n) ast

eval :: Program -> Stack -> IO ()
eval program stack = do
    shuffledStack <- (sample . shuffle) stack
    case shuffledStack of
        [] -> return ()
        _  -> case Map.lookup (head shuffledStack) program of
            (Nothing) -> eval program (tail shuffledStack)
            (Just st) -> do
                newStack <- evalStatement st False shuffledStack
                eval program newStack

evalStatement :: Statement -> Keep -> Stack -> IO (Stack)
evalStatement (Again cond statement)  True  stack = evalStatement statement True stack
evalStatement (Again cond statement)  False stack = flip (evalStatement statement) stack =<< checkCondition cond stack
evalStatement (Defer cond statement)  keep  stack = checkCondition cond stack >>= \case
    True  -> return stack
    False -> evalStatement statement  keep  stack
evalStatement (Comma doExp statement) keep  stack = evalStatement statement keep =<< evalDoExp doExp stack
evalStatement (Print strs)            True  stack = do printStrs strs stack; return stack
evalStatement (Print strs)            False stack = do printStrs strs stack; return $ tail stack
evalStatement (Statement doExp)       True  stack = evalDoExp doExp stack
evalStatement (Statement doExp)       False stack = tail <$> evalDoExp doExp stack

checkCondition :: Condition -> Stack -> IO (Bool)
checkCondition (Or  expr cond)  stack = liftM2 (||) (checkExp expr stack) (checkCondition cond stack)
checkCondition (And expr cond)  stack = liftM2 (&&) (checkExp expr stack) (checkCondition cond stack)
checkCondition (Not expr)       stack = not <$> checkExp expr stack
checkCondition (Condition expr) stack = checkExp expr stack

checkExp :: Exp -> Stack -> IO (Bool)
checkExp (AST.GT l1 l2) stack = liftM2 (>)  (evalLExp l1 stack) (evalLExp l2 stack)
checkExp (AST.LT l1 l2) stack = liftM2 (<)  (evalLExp l1 stack) (evalLExp l2 stack)
checkExp (AST.GE l1 l2) stack = liftM2 (>=) (evalLExp l1 stack) (evalLExp l2 stack)
checkExp (AST.LE l1 l2) stack = liftM2 (<=) (evalLExp l1 stack) (evalLExp l2 stack)
checkExp (AST.NE l1 l2) stack = liftM2 (/=) (evalLExp l1 stack) (evalLExp l2 stack)
checkExp (AST.EQ l1 l2) stack = liftM2 (==) (evalLExp l1 stack) (evalLExp l2 stack)
checkExp (Exp l)        stack = (`elem` stack) <$> (evalLExp l stack)

evalDoExp :: DoExp -> Stack -> IO (Stack)
evalDoExp (Hash  f l)  stack = (updateStack stack) <$> liftM2 replicate (evalLExp l stack) (evalFactor f stack)
evalDoExp (DoExp l)    stack = (\n -> updateStack stack [n]) <$> (evalLExp l stack)

updateStack :: Stack -> [Int] -> Stack
updateStack startStack = foldl (\stack n -> case n < 0 of
        True  -> delete (-n) stack
        False -> stack ++ [n])
    startStack
    . filter (/= 0)

evalLExp :: LExp -> Stack -> IO (Int)
evalLExp (Add  t l) stack = liftM2 (+) (evalTerm t stack) (evalLExp l stack)
evalLExp (Sub  t l) stack = liftM2 (-) (evalTerm t stack) (evalLExp l stack)
evalLExp (LExp t)     stack = evalTerm t stack

evalTerm :: Term -> Stack -> IO (Int)
evalTerm (Mul  f1 f2) stack = liftM2 (*) (evalFactor f1 stack) (evalFactor f2 stack)
evalTerm (Div  f1 f2) stack = liftM2 div (evalFactor f1 stack) (evalFactor f2 stack)
evalTerm (Mod  f1 f2) stack = liftM2 mod (evalFactor f1 stack) (evalFactor f2 stack)
evalTerm (Term f)     stack = evalFactor f stack

evalFactor :: Factor -> Stack -> IO (Int)
evalFactor (Par l)        stack = evalLExp l stack
evalFactor (Neg l)        stack = negate <$> evalLExp l stack
evalFactor (Factor i)     _     = return i
evalFactor (FuncFact f l) stack = evalFuncFact f l stack
evalFactor Read           _     = do
    c <- hLookAhead stdin
    if isDigit c
        then read <$> getNum
        else case c of
            '-' -> do
                hGetChar stdin
                n <- hLookAhead stdin
                if isDigit n
                    then (negate . read) <$> getNum
                    else return $ ord c
            _   -> ord <$> hGetChar stdin

getNum :: IO ([Char])
getNum = do
    c <- hLookAhead stdin
    if isDigit c
        then liftM2 (:) (hGetChar stdin) getNum
        else case c of
            '\n' -> do hGetChar stdin; return []
            _    -> return []

evalFuncFact :: Function -> LExp -> Stack -> IO (Int)
evalFuncFact N l stack = length <$> filterM (\x -> (x ==) <$> (evalLExp l stack)) stack
evalFuncFact U l stack = do
    c <- evalFuncStr U l stack
    return $ if isDigit c
        then digitToInt c
        else 0

evalFuncStr :: Function -> LExp -> Stack -> IO (Char)
evalFuncStr U l stack = chr <$> evalLExp l stack

printStrs :: Strs -> Stack -> IO ()
printStrs (Concat str strs) stack = do putStr =<< getStr str stack; printStrs strs stack
printStrs (Strs str)        stack = putStrLn  =<< getStr str stack

getStr :: Str -> Stack -> IO (String)
getStr (Str str)     stack = return str
getStr (Num lExp)    stack = show <$> evalLExp lExp stack
getStr (FuncStr f l) stack = sequence [evalFuncStr f l stack]
