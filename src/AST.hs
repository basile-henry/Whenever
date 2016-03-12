module AST where

data Function   = N
                | U
                    deriving (Show, Eq)
data Str        = Str String
                | FuncStr Function LExp
                | Num LExp
                    deriving (Show, Eq)
data Strs       = Concat Str Strs
                | Strs Str
                    deriving (Show, Eq)
data Factor     = Par LExp
                | Read
                | Neg LExp
                | FuncFact Function LExp
                | Factor Int
                    deriving (Show, Eq)
data Term       = Mul Factor Factor
                | Div Factor Factor
                | Mod Factor Factor
                | Term Factor
                    deriving (Show, Eq)
data LExp       = Add Term Term
                | Sub Term Term
                | LExp Term
                    deriving (Show, Eq)
data Exp        = GT LExp LExp
                | LT LExp LExp
                | GE LExp LExp
                | LE LExp LExp
                | NE LExp LExp
                | EQ LExp LExp
                | Exp LExp
                    deriving (Show, Eq)
data DoExp      = Hash Factor LExp
                | DoExp LExp
                    deriving (Show, Eq)
data Condition  = Or Exp Condition
                | And Exp Condition
                | Not Exp
                | Condition Exp
                    deriving (Show, Eq)
data Statement  = Again Condition Statement
                | Defer Condition Statement
                | Comma DoExp Statement
                | Print Strs
                | Statement DoExp
                    deriving (Show, Eq)
data Line       = Line Int Statement
                    deriving (Show, Eq)
type AST        = [Line]