data Expr =
  Const Int |
  Expr FName Expr |
  Expr :+: Expr
  -- Expr :-: Expr |
  -- Expr :*: Expr |
  -- Expr :/: Expr

infixl 6 +
infixl 6 -
infixl 7 *
infixl 7 /
infixl 8 **

instance Show Expr where
  showsPrec p e0 =
    case e0 of
     Const n -> shows n
     x :+: y -> showParen (p >= 6) $ (showsPrec 6 x) . (" :+: " ++) . (showsPrec 6 y)
    --  x ++ "+" ++  y -> showParen (p >= 6) $ (showsPrec 6 x) . (" + " ++) . (showsPrec 6 y)
     x "-" y -> showParen (p >= 6) $ (showsPrec 6 x) . (" - " ++) . (showsPrec 6 y)
     x "*" y -> showParen (p >= 7) $ (showsPrec 7 x) . (" * " ++) . (showsPrec 7 y)
     x "/" y -> showParen (p >= 7) $ (showsPrec 7 x) . (" / " ++) . (showsPrec 7 y)
     x "**" y -> showParen (p >= 7) $ (showsPrec 7 x) . (" / " ++) . (showsPrec 7 y)