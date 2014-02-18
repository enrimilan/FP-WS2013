--5.1

divAndConquer :: (p -> Bool) -> (p -> s) -> (p -> [p]) -> (p -> [s] -> s) -> p -> s
divAndConquer ind solve divide combine initProblem
	= dac initProblem
	where dac problem
		| ind problem = solve problem
		| otherwise = combine problem (map dac (divide problem))

---a)

quickSort :: Ord a => [a] -> [a]
quickSort = divAndConquer indq solveq divideq combineq

--Liefert True, wenn die als Argument uebergebene Probleminstanz nicht mehr teilbar ist, False sonst
indq :: [a] -> Bool
indq p = length p <2

--Liefert die Loesung zu einer nicht mehr teilbaren Probleminstanz
solveq :: [a] -> [a]
solveq p=p

--Liefert eine Liste von Instanzen von Teilproblemen, wenn die als Argument uebergebene Probleminstanz teilbar ist
divideq :: Ord a => [a] -> [[a]]
divideq [] = [[],[]]
divideq a = [[x|x<-tail a,x<=head a],[x|x<-tail a,x>head a]]

--Konstruiert aus der Instanz des Ausgangsproblems und den Loesungen seiner Teilprobleme die Loesung des Ausgangsproblems
combineq :: [a] -> [[a]] -> [a]
combineq [] [x,y] = x ++ y
combineq p [] = [head p]
combineq p [x,y] = x ++ [head p] ++ y

---b)

binom :: (Integer,Integer) -> Integer
binom = divAndConquer indb solveb divideb combineb

--Liefert True, wenn die als Argument uebergebene Probleminstanz nicht mehr teilbar ist, False sonst
indb :: (Integer,Integer) -> Bool
indb (n,k) = (n==k || k<=2 || k>n) && k>=0 && n>=0

--Liefert die Loesung zu einer nicht mehr teilbaren Probleminstanz
solveb :: (Integer,Integer) -> Integer
solveb (n,k)
	| k>n 		= 0
	| k==1		= n
	| k==2 		= div (n*(n-1)) 2
	| otherwise = 1

--Liefert eine Liste von Instanzen von Teilproblemen, wenn die als Argument uebergebene Probleminstanz teilbar ist
divideb :: (Integer,Integer) -> [(Integer,Integer)]
divideb (n,k) = [(n-1,k-1),(n-1,k)]

--Konstruiert aus der Instanz des Ausgangsproblems und den Loesungen seiner Teilprobleme die Loesung des Ausgangsproblems
combineb :: (Integer,Integer) -> [Integer] -> Integer
combineb _ [] = 0
combineb _ [a,b] = a + b


--5.2

data Tree a = Nil | Node a (Tree a) (Tree a) deriving (Eq,Show)

--Die Funktion map fuer Baeume
tmap :: (a -> a) -> (Tree a) -> (Tree a)
tmap f Nil = Nil
tmap f (Node a x y) = Node (f a) (tmap f x) (tmap f y)

--Die Funktion zipWith fuer Baeume
tzw :: (a -> a -> a) -> (Tree a) -> (Tree a) -> (Tree a)
tzw f x Nil = Nil
tzw f Nil y = Nil
tzw f (Node a x y) (Node b z w) = Node (f a b) (tzw f x z) (tzw f y w)

--Die Funktion fold fuer Baeume
tfold :: (a -> a -> a -> a) -> a -> (Tree a) -> a
tfold f i Nil = i
tfold f i (Node a x y) = f a (tfold f i x) (tfold f i y)