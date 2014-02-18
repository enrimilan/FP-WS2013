--1.1
-- Berechnet bei gegebener Teilnehmerzahl die Anzahl der Spiele,
-- bis der Sieger bestimmt wurde. Die Teilnehmerzahl muss eine 2er-Potenz sein.
anzTurnierSpiele :: Integer -> Integer
anzTurnierSpiele n
	| n==2						= 1
	| n<0 						= -1 
	| ist2erPotenz n == False 	= -1
	| otherwise 				= div n 2 + anzTurnierSpiele (div n 2)

-- Gibt fuer eine gegebene Zahl True zurueck, falls diese eine 2er-Potenz ist, sonst False.
ist2erPotenz :: Integer -> Bool
ist2erPotenz n
	| n==0				= False
	| n==1 				= True
	| mod n 2 == 1	 	= False
	| otherwise			= ist2erPotenz (div n 2)


--1.2
-- Berechnet bei gegebener Teilnehmerzahl die Anzahl der Spiele,
-- bis der Sieger bestimmt wurde. Die Teilnehmerzahl muss am Anfang gerade sein.
-- Ist die Zahl nach einer Runde ungerade, so darfder beste Verlierer in der 
-- naechsten Runde wieder mitantreten.
anzAllgTurnierSpiele :: Integer -> Integer
anzAllgTurnierSpiele n
	| n==2				= 1
	| mod n 2 == 1	 	= -1
	| n<=0				= -1
	| otherwise  		= div n 2 + anzAllgTurnierSpiele (div n 2 + mod (div n 2) 2)

	
--1.3
-- Liefert True zurueck falls es sich um eine Vollkommene Zahl handelt, sonst false.
istVollkommen :: Integer -> Bool
istVollkommen 0 = False
istVollkommen n
	| n<0 							= error "Argument muss positiv sein"
	| n == sum (echteTeiler n) 		= True
	| otherwise 					= False

-- Gibt eine Liste mit allen echten Teilern einer Zahl n zurueck.
-- n selbst ist kein echter Teiler.
echteTeiler :: Integer -> [Integer]
echteTeiler n
	| n<=0			= []
	| otherwise 	= [a |  a <- [1..n-1], mod n a == 0]

	
--1.4
-- Liefert die Indexposition in a, an der b in a zum ersten Mal als Teilzeichenreihe vorkommt und beginnt.
erstesTzrVork :: String -> String -> Int
erstesTzrVork _ [] = 0
erstesTzrVork a b = gibIndexZurueck a b a b 0 (length b) 0

-- Hilfsfunktion, die ersten 2 Argumente veraendern sich mit jedem
-- rekursiven Aufruf, die anderen 2 bleiben immer Konstant, das naechste
-- Argument ist ein Zaehler, der am Ende immer neu gesetzt wird, dann die
-- Laenge der zu durchsuchenden Teilzeichenreihe, und noch der Index, der, 
-- falls eine Teilzeichenreihe gefunden wurde, zurueckgegeben wird.
gibIndexZurueck :: String -> String -> String -> String -> Int -> Int -> Int -> Int
gibIndexZurueck [] _ _ _ _ _ _  = -1
gibIndexZurueck _ [] _ _ _ _ _  = -1
gibIndexZurueck (x:xs) (y:ys) a b n l i
	| i>=length a 		= -1
	| n==l-1 && x==y	= i-n
	| x==y 				= gibIndexZurueck xs ys a b (n+1) l (i+1)
	| otherwise 		= gibIndexZurueck xs b a b 0 l (i+1)


--1.5
-- Liefert diejenige Indexposition, an der b in a zum letzten Mal als Teilzeichenreihe vorkommt und beginnt.
letztesTzrVork :: String -> String -> Int
letztesTzrVork a [] = length a
letztesTzrVork a b 
	| erstesTzrVork a b == -1		= -1 
	| otherwise						= length a - (erstesTzrVork (reverse a) (reverse b)) - length b