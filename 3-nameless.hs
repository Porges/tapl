type Var = String
data Named
	= App Named Named
	| Lam Var Named
	| Ref Var

-- Exercise 6.1.1
c0 = Lam "s" $ Lam "z" $ Ref "z"
c2 = Lam "s" $ Lam "z" $ (Ref "s") `App` ((Ref "s") `App` (Ref "z"))
-- this is stated incorrectly in the book:
plus = Lam "m" $ Lam "n" $ Lam "s" $ Lam "z" $ Ref "m" `App` Ref "s" `App` (Ref "n" `App` Ref "z" `App` Ref "s")
fix = Lam "f" $ (Lam "x" $ Ref "f" `App` (Lam "y" $ (Ref "x" `App` Ref "x") `App` Ref "y")) `App` (Lam "x" $ Ref "f" `App` (Lam "y" $ (Ref "x" `App` Ref "x") `App` Ref "y"))
foo = (Lam "x" $ Lam "x" (Ref "x")) `App` (Lam "x" (Ref "x"))

-- Exercise 6.1.5 (1)
dename :: Named -> DB
dename = dename' []

type Distance = Int
data DB
	= App' DB DB
	| Lam' DB
	| Ref' Distance

-- this would make a nice esolang
instance Show DB where
	show (Ref' d) = show d
	show (Lam' db) = "[" ++ show db ++ "]"
	show (App' db1 db2) = "(" ++ show db1 ++ " " ++ show db2 ++ ")"

type Context = [Var]

distance :: Context -> Var -> Distance
distance ctx var = d' ctx var 0
	where 
		d' [] var _ = error $ "variable not found: " ++ var
		d' (x:xs) var i = if x == var then i else d' xs var (i+1)

dename' :: Context -> Named -> DB
dename' ctx (Ref v) = Ref' (distance ctx v)
dename' ctx (Lam v nd) = Lam' (dename' (v:ctx) nd)
dename' ctx (App n1 n2) = App' (dename' ctx n1) (dename' ctx n2)