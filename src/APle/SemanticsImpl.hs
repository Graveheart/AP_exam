module SemanticsImpl where

import AST
import Control.Monad

import qualified Data.Map as Map

-- Put your implementation of the rewrite engine here. Feel free to
-- define your own auxilary functions as needed, but do not modify
-- the type definitions and declarations of the required individual
-- functions from what's specified here and in the assignment text.

---- Global monad and related functions

-- initialContext :: Context
-- initialContext = (Map.empty, initialPEnv)
--   where initialPEnv =
--           Map.fromList [ ("===", equals)
--                        , ("<", lessthan)
--                        , ("+", plus)
--                        , ("*", times)
--                        , ("-", minus)
--                        , ("%", modulus)
--                        , ("Array", mkArray)
--                        ]

type GEnv = [Rule]

newtype Global a = Global {runGlobal :: GEnv -> Either (Maybe ErrMsg) a}

instance Monad Global where
  -- return :: (Monad m) => x -> mx
  return v = Global $ \ _ -> Right v
  -- (>>=) :: (Monad t) => t x -> (x -> m y) -> m y
  -- t >>= f =
  (Global t) >>= f = Global $ \x ->
    do
      y <- t x
      runGlobal (f y) x
  -- fail s = Global $ \ _ -> Left Just s
  -- Global t >>= f = Global $ \genv ->
  --   runGlobal (f $ t genv) genv

-- You may modify these if you want, but it shouldn't be necessary
instance Functor Global where fmap = liftM
instance Applicative Global where pure = return; (<*>) = ap

getRules :: Global [Rule]
getRules = Global $ \ rules -> Right rules

failS :: Global a
failS = Global $ \ _ -> Left Nothing

failH :: ErrMsg -> Global a
failH e = Global $ \_ -> Left $ Just e

tryS :: Global a -> Global a -> Global a
tryS = undefined
-- tryS m1 m2 = case runGlobal m1 of


---- Local monad and related functions

type LEnv = [(VName, Term)]

newtype Local a = Local {runLocal :: LEnv -> Global (a, LEnv)}

instance Monad Local where
  return v =  Local $ \ lenv -> Global $ \ _ -> Right (v, lenv)
  Local t >>= f = Local $ \ x ->
    do
      (a, lenv1) <- t x
      (b, lenv2) <- runLocal (f a) lenv1
      return (b, lenv2)
  -- fail s = SubsM $ \ _ -> Left s

instance Functor Local where fmap = liftM
instance Applicative Local where pure = return; (<*>) = ap

inc :: Global a -> Local a
inc = undefined
-- inc global = Local lenv

getLenv :: Local LEnv
getLenv = undefined
-- getLenv = Local $ \lenv -> lenv

askVar :: VName -> Local Term
-- askVar = undefined
askVar vname =
  -- lenv <- getLenv
  Local $ \ lenv ->
    do
      m <- Map.fromList lenv
      case Map.lookup vname m of
        -- Nothing -> Local $
        Just term -> Local $ TVar vname
  -- case lookupShape (mShapes ctx) key of
  --   Nothing -> Salsa $ \_ -> Left ("shape " ++ key ++ " doesn't exist")
  --   Just shapeInfo -> return $ positionX shapeInfo
  -- Local (find "id") (find "name")
  -- m = Map.fromList lenv
  -- User (find "id") (find "name")
  -- where
  --   m = Map.fromList xs
  --   find name = fromMaybe "" $ Map.lookup name m
  --   case find vname lenv of
  --     Nothing -> do
  --       failH $ "Variable " ++ vname ++ " does not exist"
  --       return Local lenv
  --     (Just var) -> Local $ lenv ++ [var]



tellVar :: VName -> Term -> Local ()
tellVar = undefined
-- tellVar vname term = do
--   lenv <- getLenv
--   m <- Map.fromList lenv
--   case Map.lookup vname m of
--     Nothing -> Local $ \ _ -> Right lenv ++ [(vname, def)]
--     Just _ -> Local $ \_ -> Left ("shape " ++ ident ++ " already exists")

-- tellVar v t = Local $ \ (env,_) -> Right Map.insert name val env

---- Matching and instantiation

matchTerm :: Term -> Term -> Local ()
matchTerm (TNum num) = return num

instTerm :: Term -> Local Term
instTerm = undefined

---- Conditions and rule aplication

evalCond :: PName -> [Term] -> Global [Term]
-- evalCond (Number num) = return $ IntVal num
evalCond = undefined

applyRule :: Rule -> Term -> Global Term
applyRule = undefined

---- Single-step term rewriting

rewriteTerm :: Term -> Global Term
rewriteTerm = undefined

---- Top-level interaction

processCmd :: Cmd -> GEnv -> (Rsp, GEnv)
processCmd = undefined
-- Global $ \x ->
--     do
--       y <- t x
--       runGlobal (f y) x
