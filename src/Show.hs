module Show where
import FormulaAST

-- showFormula :: Formula->String
showFormula _ (Atom name) = showString name
showFormula p (Not formula) = showParen (p > 3) $
  showString "\\+ " . showFormula 3 formula
showFormula p (And lhs rhs) = showParen (p > 2) $
  showFormula 3 lhs . showString " /\\ " . showFormula 2 rhs
showFormula p (Or lhs rhs) = showParen (p > 1) $
  showFormula 2 lhs . showString " \\/ " . showFormula 1 rhs
showFormula p (Imp lhs rhs) = showParen (p > 0) $
  showFormula 1 lhs . showString " -> " . showFormula 0 rhs

main = showFormula $ Imp (And (Imp (Atom "A") (Atom "B")) (Atom "A")) (Atom "B")