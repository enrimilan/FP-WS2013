import Data.List
--6.1

data Nat = Z | S Nat deriving Show

instance Eq Nat where 
	Z == Z 			= True
	(S a) == (S b)  = (a==b)
	_ == _			= False
	
instance Ord Nat where
	Z <=Z			= True
	Z <= (S a) 		= True
	(S a) <= (S b)  = (a<=b)
	_ <= _ 			= False
	
instance Num Nat where
	negate Z   		= Z
	negate (S a) 	= Z
	abs Z 			= Z
	abs (S a)		= (S a)
	signum Z 		= Z
	signum (S a)	= (S Z)
	Z + a 	 		= a
	(S a) + b 		= S (a + b)
	Z - a 			= Z
	a - Z 			= a
	(S a) - (S b) 	= a - b
	_ * Z 			= Z
	Z * _			= Z
	(S a) * (S b) 	=  a * (S b) + (S b)
	fromInteger i 
		| i<=0		= Z
		| otherwise = S (fromInteger (i-1)) 
		

--6.2

class Ord a => SimNf a where
	similar :: a -> a -> Bool
	normalform :: a -> a
	
data Tree b = Null | Node b (Tree b) (Tree b) deriving (Eq,Ord,Show)

instance SimNf Integer where
	similar a b = sum (getDigits (abs a)) == sum (getDigits (abs b))
	normalform a 
		| a<0 			= (-1) * normalform (abs a)
		| otherwise 	= digitsToNumber (sort (getDigits a))
	
--Hilfsfunktion, gibt die Liste der Ziffern einer Zahl zurueck.
getDigits :: Integer -> [Integer]
getDigits 0 = []
getDigits x = getDigits (x `div` 10) ++ [x `mod` 10]

--Hilfsfunktion, macht das Umgekehrte zur Obigen.
digitsToNumber :: [Integer] -> Integer
digitsToNumber = foldl add 0
   where add a b = 10*a + b

instance (SimNf b) => SimNf (Tree b) where
	similar a b = (getMarken a == getMarken b) && (selbeStruktur a b)
	normalform a = a

--Gibt True zurueck, falls die Baeume von der Struktur her gleich sind, ohne die Eintraege zu beruecksichtigen.
selbeStruktur :: (Tree a) -> (Tree a) -> Bool
selbeStruktur Null Null  = True
selbeStruktur Null b	 = False
selbeStruktur b Null 	 = False
selbeStruktur (Node a a1 a2) (Node b b1 b2) = (selbeStruktur a1 b1) && (selbeStruktur a2 b2)

--Gibt eine Liste mit den Eintraegen des Baumes zurueck, und zwar aufsteigend sortiert.
getMarken Null = []
getMarken (Node t t1 t2) = sort (t:(getMarken t1) ++ (getMarken t2))