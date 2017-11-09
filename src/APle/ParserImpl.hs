module ParserImpl
where

import Data.Char
import Text.ParserCombinators.ReadP
import Control.Applicative ()
import AST

-- Term ::= vname
--       | number
--       | fname ‘(’ Termz ‘)’
--       | Term oper Term
--       | ‘(’ Term ‘)’
-- Termz ::= ε
-- | Terms
-- Terms ::= Term
-- | Term ‘,’ Terms
-- Cond ::= pname ‘(’ Termz ‘)’
-- | pname ‘(’ Termz ‘;’ Terms ‘)’
-- Conds ::= Cond
-- | Cond ‘,’ Conds
-- Rule ::= Term ‘=’ Term ‘.’
-- | Term ‘=’ Term ‘|’ Conds ‘.’
-- Cmd ::= Rule
-- | Term ‘?’
-- | Term ‘??’
-- Cmds ::= ε
-- | Cmd Cmds

-- Term ::= Term1 TermOpt
-- TermOpt ::= '<=' Term1 TermOpt | '<' Term2 TermOpt | ϵ
-- Term1 ::= Term2 Term1Opt
-- Term1Opt ::= '+' Term2 Term1Opt | '-' Term2 Term1Opt | ϵ
-- Term2 ::= Term3 Term2Opt
-- Term2Opt ::= '*' Term3 Term2Opt | ϵ
-- Term3 ::= Terminal Term3Opt
-- Term3Opt ::= '**' Terminal Term3Opt | ϵ
-- Terminal ::= vname
--         | number
--         | fname ‘(’ Termz ‘)’
--         | ‘(’ Term ‘)’

cmds::ReadP [Cmd]
cmds = (do r <- cmd
           return [r])
    +++ (do r <- cmd
            rs <- cmds
            return $ r:rs)

cond :: ReadP Cond
cond = do
            n <- name
            _ <- operator "("
            t1 <- terms
            _ <- operator ";"
            t2 <- terms
            _ <- operator ")"
            if not (null t2) then return $ Cond n t1 t2
                            else pfail
    +++
        do
            n <- name
            t <- termzBrackets
            return $ Cond n t []


conds :: ReadP [Cond]
conds = sepBy cond (operator ",")

cmd :: ReadP Cmd
cmd = (do r <- rule
          return $ CRule r)
    +++ (do t <- term
            _ <- operator "?"
            return $ CQuery t False)

term :: ReadP Term
term = do
    t1 <- term1
    termOpt t1

termOpt :: Term -> ReadP Term
termOpt t = do
                op <- operator "<="
                t1 <- term1
                termOpt $ TFun op [t,t1]
        <++ do
                op <- operator "<"
                t1 <- term1
                termOpt $ TFun op [t,t1]
        <++ return t

term1 :: ReadP Term
term1 = do
    t2 <- term2
    termOpt1 t2

termOpt1 :: Term -> ReadP Term
termOpt1 t = do
                op <- operator "+"
                t2 <- term2
                termOpt1 $ TFun op [t,t2]
        <++ do
                op <- operator "-"
                t2 <- term2
                termOpt1 $ TFun op [t,t2]
        <++ return t

term2 :: ReadP Term
term2 = do
    t3 <- term3
    termOpt2 t3

termOpt2 :: Term -> ReadP Term
termOpt2 t = do
                op <- operator "*"
                t3 <- term3
                termOpt2 $ TFun op [t,t3]
        <++ return t

term3 :: ReadP Term
term3 = do
    t4 <- terminal
    termOpt3 t4

termOpt3 :: Term -> ReadP Term
termOpt3 t = do
                op <- operator "**"
                t4 <- terminal
                termOpt3 $ TFun op [t,t4]
        <++ return t

terminal :: ReadP Term
terminal = (do
            vname <- name
            return $ TVar vname)
    +++ number
    +++ do
            fname <- name
            t <- termzBrackets
            return $ TFun fname t
    +++ (do
            _ <- operator "("
            t <- term
            _ <- operator ")"
            return t)


terms :: ReadP [Term]
terms = sepBy term (operator ",")

-- termz :: ReadP (Maybe [Term])
-- termz = do
--             t <- terms
--             return $ Just t
--     +++ return Nothing

termzBrackets :: ReadP [Term]
termzBrackets = do
                _ <- operator "("
                t <- terms
                _ <- operator ")"
                return t

rule :: ReadP Rule
rule = (do t1 <- term
           _ <- operator "="
           t2 <- term
           _ <- operator "."
           return $ Rule t1 t2 [])
    +++ (do t1 <- term
            _ <- operator "="
            t2 <- term
            _ <- operator "|"
            c <- conds
            _ <- operator "."
            return $ Rule t1 t2 c)


operator :: String -> ReadP String
operator o = do
    skipSpaces
    opr o
    where
        opr [c] = do
            _ <- satisfy (==c)
            return o
        opr (c:cs) = do
            _ <- satisfy (==c)
            opr cs
        opr [] = pfail

-- used for both vname and pname
name :: ReadP String
name = do
    skipSpaces
    f <- satisfy isLetter
    v <- munch (\c-> isLetter c || isDigit c)
    return $ f:v

number :: ReadP Term
number = do
            skipSpaces
            m <- minus
            n <- munch1 isDigit
            return $ TNum $ m * read n
    -- +++ (do skipSpaces
            -- n <- munch1 isDigit
            -- return $ read n)

minus :: ReadP Integer
minus = do
        operator "~"
        return (-1)
      <++
        return 1


-- do not change the type!
-- parseStringTerm :: OpTable -> String -> Either ErrMsg Term
parseStringTerm :: String -> Either ErrMsg Term
parseStringTerm s =
    case readP_to_S program s of
        [(j, "")] -> Right j
        [] -> Left $ "Parse error at input -> '" ++ last (words s) ++ "' in: '" ++ s ++ "'"
        _  -> Left "Prelude.read: ambiguous parse"
    where
        program :: ReadP Term
        program = do
                    result <- term
                    skipSpaces
                    eof
                    return result

-- parseStringCmds :: OpTable -> String -> Either ErrMsg [Cmd]
parseStringCmds :: String -> Either ErrMsg [Cmd]
parseStringCmds s =
    -- case readP_to_S program ops of
    --     [(j, "")] -> Right j
    --     [] -> Left $ "Parse error at input -> '" ++ last (words s) ++ "' in: '" ++ s ++ "'"
    --     _  -> Left "Prelude.read: ambiguous parse"
    -- where
    --     program :: ReadP [Cmd]
    --     program = do
    --                 result <- optableParser
    --                 skipSpaces
    --                 eof
    --                 return result
    case readP_to_S program s of
        [(j, "")] -> Right j
        [] -> Left $ "Parse error at input -> '" ++ last (words s) ++ "' in: '" ++ s ++ "'"
        _  -> Left "Prelude.read: ambiguous parse"
    where
        program :: ReadP [Cmd]
        program = do
                    result <- cmds
                    skipSpaces
                    eof
                    return result

parseString  :: String -> Either ErrMsg [Cmd]
parseString s =
    case readP_to_S program s of
        [(j, "")] -> Right j
        [] -> Left $ "Parse error at input -> '" ++ last (words s) ++ "' in: '" ++ s ++ "'"
        _  -> Left "Prelude.read: ambiguous parse"
    where
        program :: ReadP [Cmd]
        program = do
                    result <- cmds
                    skipSpaces
                    eof
                    return result
