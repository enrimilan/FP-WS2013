import Data.List
--3.1
-- Ueberprueft, ob ein abgegebener Stimmzettel gueltig ist oder nicht.
istGueltig :: [String] -> [Int] -> Bool
istGueltig [] _ = False
istGueltig s i 
	| length i /= length s					= False
	| length (nub i) /= length s			= False
	| checkRang i (length s)==False 		= False
	| otherwise 							= True

-- Ueberprueft ob ein Element einer Liste nicht groesser ist als die Anzahl der Elemente dieser Liste.
checkRang :: [Int] -> Int -> Bool
checkRang [] _ 		= True
checkRang (i:is) l
	| i>l || i<=0			= False
	| otherwise				= checkRang is l
	

--3.2
-- Trennt die gueltigen von den ungueltigen Stimmen.
trenneStimmen :: [String] -> [[Int]] -> ([[Int]],[[Int]])
trenneStimmen s is =([i | i<-is ,istGueltig s i], [i | i<-is, istGueltig s i==False])


--3.3
-- Zaehlt fuer jede Person wie oft sie auf Rang 1 steht.
auszaehlen :: [String] -> [[Int]] -> [Int]
auszaehlen s is = [sum[1|x<- fst(trenneStimmen s is), head x ==y] | y<-[1..length s]]


--3.4
-- Gibt zurueck ob ein Kandidat mit der absoluten Mehrheit der gueltigen Stimmen gewaehlt worden ist oder nicht.
erfolgreich :: [String] -> [[Int]] -> Bool
erfolgreich _ [] = False
erfolgreich [] _ = False
erfolgreich s is = (maximum st) * 2> sum st
	where st=auszaehlen s (fst(trenneStimmen s is))


--3.5
-- Gibt den Namen des Kanditaten der die absolute Mehrheit der gueltigen Stimmen hat.
gewaehltIst :: [String] -> [[Int]] -> String
gewaehltIst s is
	| erfolgreich s is 		= gibNameZurueck s (gibIndexZurueck (auszaehlen s is) (maximum (auszaehlen s is)))
	| otherwise 			= ""

-- Gibt den Index einer Zahl zurueck wo sie zum ersten Mal in einer Liste vorkommt.
gibIndexZurueck :: [Int] -> Int -> Int
gibIndexZurueck [] _ = -1
gibIndexZurueck (x:xs) y
	| x==y 		= 1
	| otherwise = 1+ gibIndexZurueck xs y

-- Gibt das Element einer Liste von Zeichenkette an für gegebenen Index zurueck.
gibNameZurueck :: [String] -> Int -> String
gibNameZurueck [] i = ""
gibNameZurueck (x:xs) i
	| i>1		= gibNameZurueck xs (i-1)
	| otherwise = x
	

--3.6
-- Falls kein Kanditat die absolute Mehrheit hat, wird die naechste Auszaehlung vorbereitet.
vorbereiteNaechsteAuszaehlung :: [String] -> [[Int]] -> ([String],[[Int]])
vorbereiteNaechsteAuszaehlung s is
	| erfolgreich s is 	= (s, is)
	| otherwise 		= ((loesche s iss 1),(verschiebe (fst(trenneStimmen s is)) iss))
		where iss=gibIndizesZurueck (auszaehlen s is) 1 (minimum(auszaehlen s is))

-- Gibt eine Liste mit Indizes fuer die Elemente zurueck, die einen Wert gleich dem kleisten Wert dieser Liste haben.
gibIndizesZurueck :: [Int] -> Int -> Int -> [Int]
gibIndizesZurueck [] _ _ = []
gibIndizesZurueck (r:rs) i min
	| r==min 		= i : (gibIndizesZurueck rs (i+1) min)
	| otherwise 	= (gibIndizesZurueck rs (i+1) min)

-- Loescht die Kanditaten an die gegebenen Positionen.
loesche :: [String] -> [Int] -> Int -> [String]
loesche [] _ _ = []
loesche (s:ss) is i
	| elem i is = loesche ss is (i+1)
	| otherwise = s: loesche ss is (i+1)

-- Verschiebt Elemente zum Ende der Liste.
verschiebe :: [[Int]] -> [Int] -> [[Int]]
verschiebe is iss = [if elem (head x) iss then (tail x)++[head x] else x|x<-is]


--3.7
-- Bestimmt den endgueltigen Sieger.
endergebnis :: [String] -> [[Int]] -> String
endergebnis s is 
	| length s < 2										= "Ungueltiger Wahlvorschlag"
	| fst(vorbereiteNaechsteAuszaehlung s is) ==[] 		= "Kein Kandidat erfolgreich"
	| erfolgreich s is == False 						= endergebnis s (snd(vorbereiteNaechsteAuszaehlung s is))
	| otherwise 										= gewaehltIst s is