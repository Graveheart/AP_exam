module FormulaAST where

type ErrMsg = String  -- human-readable error messages
type VName = String   -- variable names
type FName = String   -- function (including operator) names
type PName = String   -- predicate names

data Term =
    TVar VName
  | TNum Integer
  | TFun FName [Term]

instance Show Term where
  showsPrec (TVar name) = showString name
  showsPrec ( TNum n) = showString n
  showsPrec p (Not formula) = showParen (p > 3) $
    showString "\\+ " . showsPrec 3 formula
  showsPrec p (And lhs rhs) = showParen (p > 2) $
    showsPrec 3 lhs . showString " /\\ " . showsPrec 2 rhs
  showsPrec p (Or lhs rhs) = showParen (p > 1) $
    showsPrec 2 lhs . showString " \\/ " . showsPrec 1 rhs
  showsPrec p (Imp lhs rhs) = showParen (p > 0) $
    showsPrec 1 lhs . showString " -> " . showsPrec 0 rhs

main = print $ Imp (And (Imp (Atom "A") (Atom "B")) (Atom "A")) (Atom "B")