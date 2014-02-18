import Data.List
--2.1
type Histogramm = [(Integer,[Char])] 

-- Zeichnet das Histogramm fuer eine gegebene Liste von Zahlen.
histo :: [Integer] -> Histogramm
histo [] = []
histo (x:xs) =  sortBy (mySort) ((x, count (x:xs) x []) : (histo (entferne (x:xs) x)))

-- Selbstdefinierte Sortierfunktion.
mySort :: (Integer, [Char]) -> (Integer, [Char]) -> Ordering
mySort (a1, b1) (a2, b2)
  | b1 < b2 = GT
  | b1 > b2 = LT
  | b1 == b2 = compare a1 a2

-- Anzahl der Vorkommen einer bestimmten Zahl wird durch eine 
-- entsprechend lange Folge des Zeichens ’x’ zurueckgegeben.
count :: [Integer] -> Integer -> [Char] -> [Char]
count [] a p = p
count (x:xs) a p 
	| x==a 			= count xs a ("x"++p)
	| otherwise	    = count xs a p
	
-- Entfernt alle Vorkommen einer gegebenen Zahl aus der Liste.
entferne :: [Integer] -> Integer -> [Integer]
entferne [] _ = []
entferne (x:xs) z
	| x==z 			= entferne xs z
	| x/=z			= x :(entferne xs z )


--2.2
-- Normalisiert eine Zeichenkette.
normalize :: String -> Integer -> String
normalize s n  
	| n>0 			= normalize2 s s n
	| otherwise 	= []

-- Hilfsfunktion, die von normalize aufgerufen wird.
normalize2 :: String -> String -> Integer -> String
normalize2 [] ys _ = ys	
normalize2 (x:xs) ys n = normalize2 xs (bestimmeVorkommenZeichen ys x n 0) n

-- Bestimmt alle Vorkommen eines Zeichens, und haengt am Ende noch falls noetig
-- mehrere Zeichen dran, oder entfernt sie.
bestimmeVorkommenZeichen :: String -> Char -> Integer -> Integer -> String
bestimmeVorkommenZeichen [] z n c 
	| c<n 			= take ( fromIntegral (n-c)) (repeat z)
	| otherwise 	= []
bestimmeVorkommenZeichen (x:xs) z n c 
	| x==z && c<n 	= x:(bestimmeVorkommenZeichen xs z n (c+1))
	| x==z && c>=n	= bestimmeVorkommenZeichen xs z n c
	| otherwise 	= x:(bestimmeVorkommenZeichen xs z n c)


--2.3
-- Prueft ob eine Zeichenkette ein Palindrom ist oder nicht.
istPalindrom :: String -> Bool
istPalindrom s 	= 	s == (reverse s)

-- Prueft ob eine Zahl ein Palindrom ist oder nicht.
istZahlPalindrom :: Integer -> Bool
istZahlPalindrom n 
	| n>=0			= istPalindrom (show n)
	| otherwise 	= error "Keine negativen Argumente"


--2.4
-- Bestimmt die Anzahl der Zahlpalindrome die zwischen 2 gegeben Zahlen liegen.
anzZahlPalindrome :: Integer -> Integer -> Integer
anzZahlPalindrome m n
	| m<0 || n<0							= -1
	| m==n && istZahlPalindrom m == True	= 1
	| m==n && istZahlPalindrom m == False 	= 0
	| m>n 									= anzZahlPalindrome n m
	| m<n && istZahlPalindrom m == True 	= 1+ (anzZahlPalindrome (m+1) n)
	| otherwise 							= anzZahlPalindrome (m+1) n


--2.5
-- Bestimm die Anzahl aller Palindrome einer fixen Laenge, 
-- die lexikographisch zwischen 2 Woertern dieser Laenge stehen.
anzPalindrome :: String -> String -> Integer
anzPalindrome "" "" = 1
anzPalindrome a b
	| a=="" || b==""									= -1
	| (gueltigerString a && gueltigerString b)==False	= -1
	| length a /= length b 								= -1
	| a>b												= anzPalindrome b a
	| a==b && istPalindrom a ==True						= 1
	| a==b && istPalindrom a ==False					= 0
	| a<b && istPalindrom a==True						= 1 + (anzPalindrome (reverse (naechstesWort (reverse a) False True)) b)
	| otherwise											= anzPalindrome (reverse (naechstesWort (reverse a) False True)) b

-- Prueft ob eine Zeichenkette gueltig ist oder nicht.
gueltigerString :: String -> Bool
gueltigerString [] = True
gueltigerString (x:xs) 
	| (elem x "abcd")==False 	= False
	| otherwise 				= gueltigerString xs

-- Gibt das lexikographisch naechste Wort zurueck.
naechstesWort :: String -> Bool -> Bool -> String
naechstesWort [] _ _ = []
naechstesWort (x:xs) carry erstes
	| x=='a' && erstes==True 	= 'b':xs
	| x=='b' && erstes==True 	= 'c':xs
	| x=='c' && erstes==True 	= 'd':xs
	| x=='d' 					= 'a': naechstesWort xs True False
	| x=='c' && carry==True		= 'd': xs
	| x=='b' && carry==True		= 'c': xs
	| x=='a' && carry==True		= 'b': xs
	| otherwise 				= x:xs