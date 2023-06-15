{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}

----------------------------------------------------------------------

-- base syntax: literals and addition

class Lit a where
    lit :: Integer -> a

class Add a where
    add :: a -> a -> a

----------------------------------------------------------------------

-- basic semantics: integers

instance Lit Integer where
    lit i = i
instance Add Integer where
    add x y = x + y
    
----------------------------------------------------------------------

-- extend syntax with subtraction

class Sub a where
    sub :: a -> a -> a

instance Sub Integer where
    sub x y = x - y

----------------------------------------------------------------------

-- additional semantics: strings

newtype Printer = P String -- string is a type synonym :-(
  deriving Show

instance Lit Printer where
    lit i = P $ show i
instance Add Printer where
    add (P x) (P y) = P $ "(" ++ x ++ "+" ++ y ++ ")"

instance Sub Printer where
    sub (P x) (P y) = P $ "(" ++ x ++ "_" ++ y ++ ")"

----------------------------------------------------------------------

-- optimize an expression to eliminate additions and subtractions of literal zeroes

-- interprets an expression to determine whether or not it is a literal zero
-- tags it Z or NZ (either way, preserving it)
-- alternatively, could tuple with a boolean

data OptZero a = Z a  -- it is a literal zero
               | NZ a -- it's not a literal zero
  deriving (Show, Functor)

instance Lit a => Lit (OptZero a) where
    lit 0 = Z (lit 0) -- it's a literal zero, so mark it
    lit i = NZ (lit i) -- it's a literal, but not zero

instance Add a => Add (OptZero a) where
    add (Z x) y = y -- left summand was a literal zero, so discard it
    add x (Z y) = x -- left summand was not a literal zero, but right summand was, so discard it
    add (NZ x) (NZ y) = NZ (add x y) -- neither summand was a literal zero

instance Sub a => Sub (OptZero a) where
    sub x (Z y) = x -- subtrahend was a literal zero, so discard it
    sub (Z x) (NZ y) = NZ (sub x y) -- minuend was a literal zero, so restore it
    sub (NZ x) (NZ y) = NZ (sub x y) -- neither minuend nor subtrahend were literal zeroes

----------------------------------------------------------------------

-- extend syntax with negation too

class Neg a where
    neg :: a -> a

instance Neg Integer where
    neg x = negate x

instance Neg Printer where
    neg (P x) = P $ "~(" ++ x ++ ")"

instance Neg a => Neg (OptZero a) where
    neg (Z x) = Z x -- argument was a literal zero, so discard the negation
    neg (NZ x) = NZ (neg x) -- argument was non-zero, so preserve negation

----------------------------------------------------------------------

-- optimize an expression by promoting negations (and eliminating double negations)

-- uses an interpretation of an expression as a function from boolean ("are we negated?") context
-- ie effectively a pair of interpretations: the negation of the expression, and the original

data DoubleNeg a = DN (Bool -> a)
instance Show a => Show (DoubleNeg a) where
  show (DN x) = show (x True, x False)

instance Lit a => Lit (DoubleNeg a) where
    lit i = DN $ \ n -> if n then lit (negate i) else lit i  -- negation of literal

instance Add a => Add (DoubleNeg a) where
    add (DN x) (DN y) = DN $ \ n -> add (x n) (y n)          -- +/- (x+y) = +/- x + +/- y

instance Sub a => Sub (DoubleNeg a) where
    sub (DN x) (DN y) = DN $ \ n -> sub (x n) (y n)          -- +/- (x-y) = (+/- x) - (+/- y)

{-
instance (Add a, Sub a) => Sub (DoubleNeg a) where
    sub (DN x) (DN y) = DN $ \ n -> add (x n) (y (not n))    -- +/- (x-y) = (+/- x) + (-/+ y)
-}

instance Neg (DoubleNeg a) where
    neg (DN x) = DN $ \ n -> x (not n)                       -- +/- (negate x) = -/+ x

----------------------------------------------------------------------

-- turn subtractions into addition and negation

data Oisin a = O (Bool, a) deriving Show

instance Lit a => Lit (Oisin a) where
    lit i = O (False, lit i)
    -- lit i = O ((i<0), lit (abs i)) -- but we want to favour eliminating negations

instance (Add a, Sub a) => Add (Oisin a) where
    add (O (u, x)) (O (v, y))
      | u==v        = O (u, add x y)
      | not u && v  = O (False, sub x y)
      | u && not v  = O (False, sub y x) 

instance (Add a, Sub a) => Sub (Oisin a) where
    sub (O (u, x)) (O (v, y)) = add (O (u, x)) (O (not v, y))

instance Neg (Oisin a) where
    neg (O (u, x)) = O (not u, x)

----------------------------------------------------------------------

data AST = Lit Integer
         | Add AST AST
         | Sub AST AST
         | Neg AST
  deriving Show

instance Lit AST where
  lit = Lit
instance Add AST where
  add = Add
instance Sub AST where
  sub = Sub
instance Neg AST where
  neg = Neg
  
class (Lit a, Add a, Sub a, Neg a) => Expr a
instance Expr AST
instance Expr a => Expr (OptZero a)

optZero :: AST -> OptZero AST
optZero (Lit 0) = Z (Lit 0)
optZero (Lit i) = NZ (Lit i)
optZero (Add x y) = case (optZero x, optZero y) of
  (Z x', y') -> y'
  (x', Z y') -> x'
  (NZ x', NZ y') -> NZ (Add x' y')
optZero (Sub x y) = case (optZero x, optZero y) of
  (x', Z y') -> x'
  (Z x', NZ y') -> NZ (Sub x' y')
  (NZ x', NZ y') -> NZ (Sub x' y')
optZero (Neg x) = case optZero x of
  Z x' -> Z x' -- argument was a literal zero, so discard the negation
  NZ x' -> NZ (Neg x') -- argument was non-zero, so preserve negation

fromAST :: Expr a => AST -> a
fromAST (Lit i) = lit i
fromAST (Add x y) = add (fromAST x) (fromAST y)
fromAST (Sub x y) = sub (fromAST x) (fromAST y)
fromAST (Neg x) = neg (fromAST x)

{-
suppose x :: (Expr a, Sub a, Neg a) => a
then (x :: OptZero Integer) = ... (x :: Integer) ...

optZero x :: OptZero AST

t :: AST
optZero t :: OptZero AST
fmap fromAST (optZero t) :: Expr a => OptZero a
fromAST t :: Expr a => a
fromAST t :: Expr a => OptZero a

in particular, fromAST t = fmap fromAST (optZero t) :: OptZero AST

-}

t :: AST
t = Add (Lit 0) (Add (Lit 3) (Lit 0))

----------------------------------------------------------------------

-- six interpretations each of some expressions

main :: IO ()
main = do
  go "add (lit 0) (add (lit 3) (lit 0))"
     (add (lit 0) (add (lit 3) (lit 0)))
  go "sub (lit 0) (sub (lit 3) (lit 0))"
     (sub (lit 0) (sub (lit 3) (lit 0)))
  go "neg (neg (add (sub (lit 7) (lit 4)) (neg (lit 0))))"
     (neg (neg (add (sub (lit 7) (lit 4)) (neg (lit 0)))))

go :: String -> (forall a . (Lit a, Add a, Sub a, Neg a) => a) -> IO ()
go s x = do
    putStrLn $ s ++ "..."
    putStrLn $ "  Printed: " ++ show (x :: Printer)
    putStrLn $ "  Evaluated: " ++ show (x :: Integer)
    putStrLn $ "  Zeroes optimized, then printed: " ++ show (x :: OptZero Printer)
    putStrLn $ "  Zeroes optimized, then evaluated: " ++ show (x :: OptZero Integer)
    putStrLn $ "  Negations optimized, then printed: " ++ show (x :: DoubleNeg Printer)
    putStrLn $ "  Negations optimized, then evaluated: " ++ show (x :: DoubleNeg Integer)
    putStrLn $ "  Both optimized, then printed: " ++ show (x :: DoubleNeg (OptZero Printer))
    putStrLn $ "  Both optimized, then evaluated: " ++ show (x :: DoubleNeg (OptZero Integer))
    putStrLn ""
    putStrLn $ "  Oisined, then printed: " ++ show (x :: Oisin Printer)
    putStrLn $ "  Oisined, then evaluated: " ++ show (x :: Oisin Integer)
    
{-

λ> main
add (lit 0) (add (lit 3) (lit 0))...
  Printed: P "(0+(3+0))"
  Evaluated: 3
  Zeroes optimized, then printed: NZ (P "3")
  Zeroes optimized, then evaluated: NZ 3
  Negations optimized, then printed: (P "(0+(-3+0))",P "(0+(3+0))")
  Negations optimized, then evaluated: (-3,3)
  Both optimized, then printed: (NZ (P "-3"),NZ (P "3"))
  Both optimized, then evaluated: (NZ (-3),NZ 3)
sub (lit 0) (sub (lit 3) (lit 0))...
  Printed: P "(0-(3-0))"
  Evaluated: -3
  Zeroes optimized, then printed: NZ (P "(0-3)")
  Zeroes optimized, then evaluated: NZ (-3)
  Negations optimized, then printed: (P "((3-0)-0)",P "(0-(3-0))")
  Negations optimized, then evaluated: (3,-3)
  Both optimized, then printed: (NZ (P "3"),NZ (P "(0-3)"))
  Both optimized, then evaluated: (NZ 3,NZ (-3))
neg (neg (add (sub (lit 7) (lit 4)) (neg (lit 0))))...
  Printed: P "~(~(((7-4)+~(0))))"
  Evaluated: 3
  Zeroes optimized, then printed: NZ (P "~(~((7-4)))")
  Zeroes optimized, then evaluated: NZ 3
  Negations optimized, then printed: (P "((4-7)+0)",P "((7-4)+0)")
  Negations optimized, then evaluated: (-3,3)
  Both optimized, then printed: (NZ (P "(4-7)"),NZ (P "(7-4)"))
  Both optimized, then evaluated: (NZ (-3),NZ 3)

-}
