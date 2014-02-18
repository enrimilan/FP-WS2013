import Data.List
import Data.Ord

--1
data Goldbach = RefutedFor Integer
				| MightHoldBecause [Integer]
				| AllWitnesses [[Integer]]
				| Irrelevant 				deriving (Eq,Ord,Show)
				
--a)

gb :: Integer -> Goldbach
gb i
	| i <= 2 || (mod i 2)==1 	= Irrelevant
	| primSummen i /= []		= MightHoldBecause (head (primSummen i))
	| otherwise 				= RefutedFor i

-- Liefert alle Paare die eine Zahl als Summe von 2 Primzahlen darstellen.
primSummen :: Integer -> [[Integer]]
primSummen n = [[x,y] | x<-[2..n], y<-[2..n], istPrim x, istPrim y, mod x 2 == 1, mod y 2 == 1, x<=y, x+y==n]

-- Liefert True fuer eine Zahl zurueck, falls diese prim ist.
istPrim :: Integer -> Bool
istPrim n = [k | k<-[2..n], (mod n k) == 0] == [n]

--b)

gbAll :: Integer -> Goldbach
gbAll i
	| gb i == Irrelevant 	= Irrelevant
	| primSummen i /= [] 	= AllWitnesses (primSummen i)
	| otherwise 			= RefutedFor i
	
--2
type Bravitaet = Integer
type Geschenkezahl = Integer
data Beobachtungspaar = Bp Bravitaet Geschenkezahl deriving (Eq,Ord,Show)
type Beobachtungsreihe = [Beobachtungspaar]

-- Laengste Liste in der Bravitaet und Geschenkezahl echt zunehmen.
pro :: Beobachtungsreihe -> Beobachtungsreihe
pro a = longestElem [c | c <- allLists1 a, korrekteListe1 c]

-- Laengste Liste in der Bravitaet echt abnimmt, jedoch Geschenkezahl echt zunimmt.
con :: Beobachtungsreihe -> Beobachtungsreihe
con a = longestElem [c | c <- allLists2 a, korrekteListe2 c]

-- Gibt die laengste Liste von allen Listen zurueck.
longestElem :: [[a]] -> [a]
longestElem = maximumBy (comparing length)

-- Gibt alle moeglichen Teillisten einer Menge von Listen zurueck.
teilListen [] = [[]]
teilListen (x:xs) = [x:teilListen | teilListen <- teilListen xs] ++ teilListen xs
	
-- Gibt die Bravitaet fuer ein Beobachtungspaar zurueck.
getBravitaet :: Beobachtungspaar -> Bravitaet
getBravitaet (Bp a b) = a

-- Gibt die Geschenkezahl fuer ein Beobachtungspaar zurueck.
getGeschenkezahl :: Beobachtungspaar -> Geschenkezahl
getGeschenkezahl (Bp a b) = b

-- Sortiert eine Liste von Beobachtungspaaren aufsteigend nach Bravitaet und Geschenkezahl.
mySort1 :: Beobachtungspaar -> Beobachtungspaar -> Ordering
mySort1 (Bp a1 b1) (Bp a2 b2)
  | a1 < a2 = LT
  | a1 > a2 = GT
  | otherwise = compare b1 b2
  
-- Sortiert eine Liste von Beobachtungspaaren absteigend nach Bravitaet, jedoch aufsteigend nach Geschenkezahl.
mySort2 :: Beobachtungspaar -> Beobachtungspaar -> Ordering
mySort2 (Bp a1 b1) (Bp a2 b2)
  | a1 < a2 = GT
  | a1 > a2 = LT
  | otherwise = compare b1 b2

-- Eine korrekte Liste ist in diesem Fall eine, in der Bravitaet und Geschenkezahl echt zunehmen.
korrekteListe1 :: Beobachtungsreihe -> Bool
korrekteListe1 [] = False
korrekteListe1 (a:[]) = True
korrekteListe1 (a:b:s)
	| getBravitaet b> getBravitaet a && getGeschenkezahl b> getGeschenkezahl a 		= korrekteListe1 (b:s)
	| otherwise 																	= False
	
-- Eine korrekte Liste ist in diesem Fall eine, in der Bravitaet echt abnimmt, jedoch Geschenkezahl echt zunimmt.
korrekteListe2 :: Beobachtungsreihe -> Bool
korrekteListe2 [] = False
korrekteListe2 (a:[]) = True
korrekteListe2 (a:b:s)
	| getBravitaet b< getBravitaet a && getGeschenkezahl b> getGeschenkezahl a 		= korrekteListe2 (b:s)
	| otherwise 																	= False

-- Liefert alle moeglichen Listen ( korrekte und nicht korrekte ), die sortiert sind.
allLists1 :: Beobachtungsreihe -> [Beobachtungsreihe]
allLists1 a = concat[teilListen b | b<-(sequence (groupBy (\x y -> getBravitaet x == getBravitaet y) (sortBy (mySort1) a)))]

-- Liefert alle moeglichen Listen ( korrekte und nicht korrekte ), die sortiert sind.
allLists2 :: Beobachtungsreihe -> [Beobachtungsreihe]
allLists2 a = concat[teilListen b | b<-(sequence (groupBy (\x y -> getBravitaet x == getBravitaet y) (sortBy (mySort2) a)))]