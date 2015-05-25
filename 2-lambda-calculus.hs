{-# LANGUAGE RankNTypes #-} -- for explicit 'forall'

import Prelude hiding (and,or,not,fst,snd,head,tail)

-- here I'm using types to help guide the implementation, so you can't mix booleans and numbers
-- turns out the lambda calculus is lots easier if you do ;)

-- Section 5.2
newtype CBool = B { appB :: forall a. a -> a -> a }
instance Show CBool where show = show . realBool

tru = B $ \t f -> t
fls = B $ \t f -> f

test :: CBool -> a -> a -> a
test (B b) = \t f -> b t f

-- Exercise 5.2.1 
and, or :: CBool -> CBool -> CBool
and (B b) (B c) = B $ b c (appB fls)
or (B b) (B c) = B $ b (appB tru) c

not :: CBool -> CBool
not (B v) = B $ v (appB fls) (appB tru)
--

newtype CPair a b = P { appP :: forall c. (a -> b -> c) -> c }
instance (Show a, Show b) => Show (CPair a b) where show p = appP p (\x y -> "(" ++ show x ++ ", " ++ show y ++ ")")

pair :: a -> b -> CPair a b
pair f s = P $ \b -> b f s

-- the book uses tru and fls more generally here (as const and flip const)
fst :: CPair a b -> a
fst p = appP p (\x y -> x)

snd :: CPair a b -> b
snd p = appP p (\x y -> y)

-- Church numerals
newtype CNum = C { appC :: forall a. (a -> a) -> a -> a }
instance Show CNum where show = show . realNat

c0, c1, c2, c3 :: CNum
c0 = C $ \_ z -> z
c1 = C $ \s z -> s z
c2 = C $ \s z -> s (s z)
c3 = C $ \s z -> s (s (s z))

scc :: CNum -> CNum
scc (C n) = C $ \s z -> s (n s z)

-- Exercise 5.2.2
scc' :: CNum -> CNum
scc' (C n) = C $ \s z -> n s (s z)
--

plus, times :: CNum -> CNum -> CNum

plus (C m) (C n) = C $ \s z -> m s (n s z)
times (C m) n = m (plus n) c0

-- Exercise 5.2.3
times' :: CNum -> CNum -> CNum
times' (C m) (C n) = C $ m . n
--

-- Exercise 5.2.4
pow :: CNum -> CNum -> CNum
pow (C m) (C n) = C $ n m
--

iszro :: CNum -> CBool
iszro (C m) = B $ m (const $ appB fls) (appB tru)

zz :: CPair CNum CNum
zz = pair c0 c0

ss :: CPair a CNum -> CPair CNum CNum
ss p = pair (snd p) (scc (snd p))

prd :: CNum -> CNum
prd m = fst (appC m ss zz)

-- Exercise 5.2.5
sub :: CNum -> CNum -> CNum
sub m n = appC n prd m
--

-- Exercise 5.2.7
equal :: CNum -> CNum -> CBool
equal m n = (iszro m `and` iszro n) `or`
	(not (iszro m) `and` not (iszro n) `and` (prd m `equal` prd n))
--

-- Exercise 5.2.8
newtype CList a = L { appL :: forall b. (a -> b -> b) -> b -> b }
instance Show a => Show (CList a) where show = show . realList 

nil :: CList a
nil = L $ \c n -> n

isNil :: CList a -> CBool
isNil (L l) = l (\_ _ -> fls) tru

cons :: a -> CList a -> CList a
cons a (L l) = L $ \c n -> c a (l c n)

head :: CList a -> a
head (L l) = l (\h _ -> h) (error "empty list")

tail :: CList a -> CList a
tail (L l) = fst (l (\h p -> pair (snd p) (cons h (snd p))) (pair nil nil))
--

realBool :: CBool -> Bool
realBool (B b) = b True False

churchBool :: Bool -> CBool
churchBool b = if b then tru else fls

realNat :: CNum -> Integer
realNat (C n) = n (+1) 0

churchNat :: Integer -> CNum
churchNat 0 = c0
churchNat n = scc (churchNat (n-1))

realList :: CList a -> [a]
realList (L l) = l (:) []

churchList :: [a] -> CList a
churchList [] = nil
churchList (x:xs) = cons x (churchList xs)

-- Can't use the one from the book in our metalanguage Haskell, so lifted this from StackOverflow
newtype Mu a = Mu (Mu a -> a)
y f = (\h -> h $ Mu h) (\x -> f . (\(Mu g) -> g) x $ x)

-- Exercise 5.2.11.
sumList :: CList CNum -> CNum
sumList l = y (\f l -> test (isNil l) c0 (plus (head l) (f (tail l)))) l