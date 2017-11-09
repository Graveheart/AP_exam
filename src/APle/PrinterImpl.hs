module PrinterImpl where

import AST
import Data.List(intercalate)
-- do not change the type!
-- printTerm :: OpTable Term -> String
printTerm (TVar v) = show v
printTerm (TNum n) = show n
printTerm (TFun f t) = f++"("++ intercalate ", " (map printTerm t) ++")"

-- getOperatorGroup :: FName -> OpTable -> [FName]
-- getOperatorGroup f opTable = do

lookup :: String -> [(Fixity, [FName])] -> [FName]
lookup _ [] = [""]
lookup x zs = map zs (\z->)

  then []
  else tail [b | (a,b) <- zs, a==x]

-- lookupString :: String -> [(String,String)] -> String
-- lookupString _ [] = "Not found"
-- lookupString x zs = if (notFound x zs)
--   then "Not found"
--   else (head [b | (a,b) <- zs, (a==x)])




