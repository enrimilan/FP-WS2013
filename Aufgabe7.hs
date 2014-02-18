import Data.List
--7.1
class Ord a => SimNf a where
	similar :: a -> a -> Bool
	normalform :: a -> a
	
newtype NewString = Nstr String deriving (Eq,Ord,Show)

data ChSeq = Empty
	| Head Char ChSeq deriving (Eq,Ord,Show)

instance SimNf NewString where
	similar (Nstr a) (Nstr b) = ((count a 'a') == (count b 'a')) && ((count a 'e') == (count b 'e')) && ((count a 'i') == (count b 'i')) && ((count a 'o') == (count b 'o')) && ((count a 'u') == (count b 'u'))
	normalform (Nstr a) = Nstr (normal a)
	
instance SimNf ChSeq where
	similar x y = ((count (convert x) 'a') == (count (convert y) 'a')) && ((count (convert x) 'e') == (count (convert y) 'e')) && ((count (convert x) 'i') == (count (convert y) 'i')) && ((count (convert x) 'o') == (count (convert y) 'o')) && ((count (convert x) 'u') == (count (convert y) 'u'))
	normalform a = convert1 (normal (convert a))
	
-- Wandelt ChSeq in einen String um.
convert :: ChSeq -> String
convert Empty = ""
convert (Head c a) = c: (convert a)

-- Wandelt String in ChSeq um.
convert1 :: String -> ChSeq
convert1 "" = Empty
convert1 (s:ss) = Head s (convert1 ss)

-- Zaehlt wie oft ein Zeichen vorkommt.
count :: String -> Char -> Int
count [] _ = 0
count (s:ss) c
	| s==c 		= 1+ count ss c
	| otherwise = count ss c
	
-- Gibt die Normalform eines Strings zurueck.
normal :: String -> String
normal [] = []
normal (s:ss)
	| elem s ['a','e','i','o','u'] 	= sort (s: normal ss)
	| otherwise 					= normal ss


--7.2
type Movie = (Title,Regisseur,MainActors,ReleaseDate,Genre,SalesPrice)
type Title = String
type Regisseur = String
type Actor = String
type MainActors = [Actor]
type ReleaseDate = Int
data Genre = Thriller | Fantasy | ScienceFiction | Comedy deriving (Eq,Ord,Show)
type SalesPrice = Int
type Database = [Movie]

instance (Show a,Show b,Show c,Show d,Show e,Show f) => Show (a,b,c,d,e,f) where 
            show (a,b,c,d,e,f) = "("++show a++","++show b++","++show c++","++show d++","++show e++","++show f++")"

-- Loescht alle Duplikate.
rm_dup :: Database -> Database
rm_dup a = rm_dup2 a []

-- Hilfsfunktion.
rm_dup2 :: Database -> Database -> Database
rm_dup2 [] _ = []
rm_dup2 (m:ms) d
	| not (elementOf m d) 	= m:(rm_dup2 ms (m:d)) 
	| otherwise 	= rm_dup2 ms d

-- Funktion die True zurueckgibt falls sich 2 Movies bis auf die Reihenfolge der Mainactors nicht unterscheiden.
same :: Movie -> Movie -> Bool
same (t1, r1, m1, rel1, g1, s1) (t2, r2, m2, rel2, g2, s2) = (t1==t2) && (r1==r2) && ((sort m1) == (sort m2)) && (rel1==rel2) && (g1==g2) && (s1==s2)

-- Liefert True falls sich ein Movie in einer Database befindet.
elementOf :: Movie -> Database -> Bool
elementOf _ [] = False
elementOf m1 (m2:ds)
	| same m1 m2 = True
	| otherwise  = elementOf m1 ds
	
-- Liefere alle Filme eines Regisseurs r aus dem Genre g, die zu einem Preis von p oder guenstiger angeboten werden.
get_rgp :: Database -> Regisseur -> Genre -> SalesPrice -> Database
get_rgp [] _ _ _ = []
get_rgp ((t, r, m, rel, g, s):ds) r1 g1 s1
	| (r==r1) && (g==g1) && (s<=s1) 	= (t, r, m, rel, g, s):(get_rgp ds r1 g1 s1)
	| otherwise 						= (get_rgp ds r1 g1 s1)
	
-- Liefere alle Regisseure zusammen mit den Filmtiteln und ihrem Genre.
get_rtg :: Database -> [(Regisseur,Title,Genre)]
get_rtg [] = []
get_rtg ((t, r, m, rel, g, s):ds) = (r,t,g): (get_rtg ds)

-- Liefere alle Schauspieler zusammen mit den Filmtiteln und dem Genre, in denen diese Schauspieler zugleich Regie gefuehrt haben.
get_atg :: Database -> [(Actor,Title,Genre)]
get_atg [] = []
get_atg ((t, r, m, rel, g, s):ds)
	| elem r m	 = (r,t,g): (get_atg ds)
	| otherwise  =  get_atg ds

-- Liefere die Titel aller Filme zusammen mit ihrem Genre und Erscheinungsjahr, in denen Schauspieler s (einer der) Hauptdarsteller war.
get_tgd :: Database -> Actor -> [(Title,Genre,ReleaseDate)]
get_tgd [] _ = []
get_tgd ((t, r, m, rel, g, s):ds) a
	| elem a m 		= (t,g,rel): (get_tgd ds a)
	| otherwise 	= get_tgd ds a
	
-- Veraendere die Preise aller Filme des Genres g, in denen Regisseur r Regie fuehrte und zugleich eine Hauptrolle innehatte, um x.
upd_dbgrai :: Database -> Genre -> Regisseur -> Int -> Database
upd_dbgrai [] _ _ _ = []
upd_dbgrai ((t, r, m, rel, g, s):ds) g1 r1 x
	| (g==g1) && (r==r1) && (elem r m) && (s+x>0) 	= (t, r, m, rel, g, s+x):(upd_dbgrai ds g1 r1 x)
	| (g==g1) && (r==r1) && (elem r m) && (s+x<=0) 	= (t, r, m, rel, g, 1): (upd_dbgrai ds g1 r1 x)
	| otherwise 									= (t, r, m, rel, g, s):(upd_dbgrai ds g1 r1 x)

-- Loesche alle Filme, in denen Schauspieler s und Schauspieler t gleichzeitig unter den Hauptdarstellern waren und die Regie nicht bei Regisseur r lag.
rm_dbaard :: Database -> Actor -> Actor -> Regisseur -> Database
rm_dbaard [] _ _ _ = []
rm_dbaard ((t, r, m, rel, g, s):ds) s1 t1 r1
	| (r/=r1) && (elem s1 m) && (elem t1 m)		= rm_dbaard ds s1 t1 r1
	| otherwise									= (t, r, m, rel, g, s):(rm_dbaard ds s1 t1 r1)

-- Liefere alle Filme, die im Jahr j oder spaeter erschienen sind und in denen Schauspieler s und Schauspieler t nicht gleichzeitig unter den Hauptdarstellern waren.
get_dbda :: Database -> ReleaseDate -> Actor -> Actor -> Database
get_dbda [] _ _ _ = []
get_dbda ((t, r, m, rel, g, s):ds) rel1 s1 t1
	| (rel1<=rel) && not ((elem s1 m) && (elem t1 m)) 	= (t, r, m, rel, g, s): (get_dbda ds rel1 s1 t1)
	| otherwise 										= get_dbda ds rel1 s1 t1
	
-- Sortiere die Datenbank aufsteigend nach Verkaufspreis.
sort_dbp :: Database -> Database
sort_dbp db = sortBy (mySort) db

-- Selbstdefinierte Sortierfunktion fuer Preis.
mySort :: Movie -> Movie -> Ordering
mySort (t1, r1, m1, rel1, g1, s1) (t2, r2, m2, rel2, g2, s2)
  | s1 <= s2 = LT
  | s1 > s2 = GT
 
-- Sortiere die Datenbank aufsteigend nach Regisseuren und die Filme eines Regisseurs aufsteigend nach Genres.
sort_dbrg :: Database -> Database
sort_dbrg db = sortBy (mySort1) (sortBy (mySort2) db)

-- Selbstdefinierte Sortierfunktion fuer Regisseur.
mySort1 :: Movie -> Movie -> Ordering
mySort1 (t1, r1, m1, rel1, g1, s1) (t2, r2, m2, rel2, g2, s2)
  | r1 <= r2 = LT
  | r1 > r2 = GT
  
-- Selbstdefinierte Sortierfunktion fuer Genre.
mySort2 :: Movie -> Movie -> Ordering
mySort2 (t1, r1, m1, rel1, g1, s1) (t2, r2, m2, rel2, g2, s2)
  | g1 <= g2 = LT
  | g1 > g2 = GT