>import Data.List
>data Person = Pers {
>	vorname,
>	nachname :: String,
>	geburtstag :: Geburtstag } deriving (Eq, Show)
>type Tag = Int
>data Monat = Jan | Feb | Mar | Apr | Mai | Jun
>			| Jul | Aug | Sep | Okt | Nov | Dez deriving (Eq, Show)
>type Jahr = Int
>data Geburtstag = Gtg Tag Monat Jahr deriving (Eq, Show)
>type PLZ = Int
>data Anschrift = Ans {
>	stadt,
>	land :: String,
>	plz :: PLZ } deriving (Eq, Show)
>newtype SozNr = Sznr Int deriving (Eq, Show)
>type Eintrag = (Person, Anschrift, SozNr)
>type Adressbuch = [Eintrag]
>data Tree = Nil
>			| Node Eintrag Tree Tree deriving (Eq, Show)


4.1
Fuegt alle Eintraege eines Adressbuchs in einen Binaerbaum hinzu.

>erzSB :: Adressbuch -> Tree
>erzSB a = erzSB2 (gibEintraegeZurueck a ([head x|x<-group (sort(gibSznrsZurueck a)), length x==1])) Nil

Hilfsfunktion, macht die eigentliche Berechnung.

>erzSB2 :: Adressbuch -> Tree -> Tree
>erzSB2 [] t = t
>erzSB2 (e:es) t = erzSB2 es (eintragen e t)

Gibt fuer einen gegebenen Eintrag die Sozialversicherungsnummer zurueck.

>getSnr :: Eintrag -> Int
>getSnr (Pers _ _ _, Ans _ _ _, Sznr s) = s

Gibt zurueck eine Liste ohne Duplikate.

>gibEintraegeZurueck :: Adressbuch ->[Int] -> Adressbuch
>gibEintraegeZurueck [] n = []
>gibEintraegeZurueck (e:es) n
>	| elem (getSnr e) n 	= e:(gibEintraegeZurueck es n)
>	| otherwise 			= gibEintraegeZurueck es n

Gibt alle Sozialversicherungsnummern zurueck.

>gibSznrsZurueck :: Adressbuch -> [Int]
>gibSznrsZurueck [] = []
>gibSznrsZurueck (e:es) = (getSnr e) : (gibSznrsZurueck es)


4.2

Prueft ob es sich um einen korrekten Binaeren Suchbaum handelt oder nicht.

>istSB :: Tree -> Bool
>istSB (Node t l r) = istSB2 t l r
>istSB Nil = True

Hilfsfunktion, macht die eigentliche Berechnung.

>istSB2 :: Eintrag -> Tree -> Tree -> Bool
>istSB2 e Nil Nil = True
>istSB2 e Nil (Node t2 l2 r2)
>	| (getSnr e < getSnr t2) 	= istSB2 t2 l2 r2
>	| otherwise 				= False
>istSB2 e (Node t1 l1 r1) Nil
>	| (getSnr e > getSnr t1) 	= istSB2 t1 l1 r1
>	| otherwise 				= False
>istSB2 e (Node t1 l1 r1) (Node t2 l2 r2)
>	| (getSnr e > getSnr t1) && (getSnr e < getSnr t2)	= (istSB2 t1 l1 r1) && (istSB2 t2 l2 r2)
> 	| otherwise 										= False


4.3

Fuegt einen Eintrag in einen Binaerbaum hinzu.

>eintragen :: Eintrag -> Tree -> Tree
>eintragen e Nil = Node e Nil Nil
>eintragen e t
>	| istSB t==False = Nil
>eintragen e (Node t l r)
>	| getSnr e > getSnr t 		= Node t l (eintragen e r)
>	| getSnr e < getSnr t		= Node t (eintragen e l) r
> 	| otherwise 				= Node t l r


4.4

Loescht einen Knoten vom Baum weg.

>loeschen :: SozNr -> Tree -> Tree
>loeschen _  Nil = Nil
>loeschen _ t
>	| (istSB t == False) = Nil
>loeschen (Sznr s) (Node e l r)
>	| getSnr e==s 	= loeschen2 s (Node e l r)
>	| getSnr e>s 	= Node e (loeschen (Sznr s) l) r
>	| getSnr e<s 	= Node e l (loeschen (Sznr s) r)
	
>loeschen2 :: Int -> Tree -> Tree
>loeschen2 s (Node e Nil Nil) = Nil
>loeschen2 s (Node e Nil r) = r
>loeschen2 s (Node e l Nil) = l
>loeschen2 s (Node e l (Node ra Nil rr))	= Node ra l rr
>loeschen2 s (Node e l (Node ra rl rr)) = Node (fst (minKey (Node ra rl rr))) l (loeschen (Sznr (snd (minKey (Node ra rl rr)))) (Node ra rl rr))

Gibt fuer einen Baum den Eintrag das mit der kleinsen Svnr zurueck, zusammen mit der Svnr selbst.

>minKey :: Tree -> (Eintrag, Int)
>minKey (Node t Nil Nil) = (t, getSnr t)
>minKey (Node _ l _) = minKey l


4.5

Gibt alle Eintraege eines Baumes zurueck, und zwar aufsteigend sortiert.

>linearisieren :: Tree -> Adressbuch
>linearisieren Nil = []
>linearisieren (Node e l r)
>	| istSB (Node e l r) == False 	= []
>	| otherwise						= sortBy (mySort) (e:((linearisieren l)++(linearisieren r)))

Selsbtdefinierte Sortierfunktion.

>mySort :: Eintrag -> Eintrag -> Ordering
>mySort a b
>  | getSnr a < getSnr b = LT
>  | getSnr a > getSnr b = GT


4.6

Filtert alle Knoten eines Baums, die Geburtstag an einem gegeben Tag haben.

>filtereNachGebTag :: Geburtstag -> Tree -> Adressbuch
>filtereNachGebTag _ Nil = []
>filtereNachGebTag (Gtg t m j) (Node e l r)
>	| j<0 || t<0 || t>31 || (m==Feb && t>28) || istSB (Node e l r) == False 	= []
>	| getGebTag e==(Gtg t m j)													= sortBy (mySort) (e:((filtereNachGebTag (Gtg t m j) l)++(filtereNachGebTag (Gtg t m j) r)))
>	| otherwise 																= sortBy (mySort) ((filtereNachGebTag (Gtg t m j) l)++(filtereNachGebTag (Gtg t m j) r))

>getGebTag :: Eintrag -> Geburtstag
>getGebTag (Pers _ _ (Gtg t m j), Ans _ _ _, Sznr _) = Gtg t m j


4.7

Filtert alle Knoten eines Baums, die Geburtstag an einem gegeben Tag haben, sowie dieselbe PLZ haben.

>filtereNachPLZundGebTag :: PLZ -> Geburtstag -> Tree -> Adressbuch
>filtereNachPLZundGebTag _ _ Nil = []
>filtereNachPLZundGebTag p (Gtg t m j) (Node e l r)
>	| j<0 || t<0 || t>31 || (m==Feb && t>28) || istSB (Node e l r) == False 	= []
>	| getGebTag e==(Gtg t m j) && getPLZ e==p									= sortBy (mySort) (e:((filtereNachPLZundGebTag p (Gtg t m j) l)++(filtereNachPLZundGebTag p (Gtg t m j) r)))
> 	| otherwise 																= sortBy (mySort) ((filtereNachPLZundGebTag p (Gtg t m j) l)++(filtereNachPLZundGebTag p (Gtg t m j) r))

>getPLZ :: Eintrag -> PLZ
>getPLZ (Pers _ _ _, Ans _ _ plz, Sznr s) = plz