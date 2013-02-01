-- Onur Yilmaz
 

-- Module description
module Dictionary  (Dictionary, empty, check, insert, merge, subs, toList, fromList) where

-- Importing other modules
import qualified Data.List as Set

-- Data definition
-- Dictionary can be Node, List of dictionary or Empty
data Dictionary a =  List [Dictionary a] | Node {root :: a, sign :: Int, children :: [Dictionary a]} |Empty

-------------------------------
-- Helper functions
-- Getter and setter functions
------------------------------

returnSign Node {root = r, sign=s, children = c}  = s

returnRoot (Node r s c)  = r
returnRoot (List t) = returnRoot (head t)

returnChildren Empty = []
returnChildren Node {root = r, sign=s, children = c}  = c

returnFirst (lst)= head lst

changeSign Node {root = r, sign=s, children = c} = Node r 1 c

------------------
-- empty function
------------------
empty = Empty

--------------------------------
-- Show instance for dictionary
--------------------------------

instance (Show a) => Show (Dictionary a ) where
		show t = " \n"  ++ showind t "" where
			-- Show Empty dictionary with indentation
			showind Empty pre  = pre
			-- Show Node dictionary with indentation
			showind (Node a b c) pre = pre ++ "    " ++ show a ++ "-->" ++ showSign b  ++"\n"++ showChildren c pre 
			-- Show List dictionary with indentation
			showind (List t) pre  -- = 
				| (length t == 0) = ""
				| (length t == 1) = showind (head t) ""
				| (length t > 1) = showind (head t) pre ++ showind (List (tail t)) ""
			showChildren [] pre = ""
			showChildren children pre = showind (head children) (pre ++ "    ")  ++ showChildren (tail children) pre

-- showSign helper function
showSign b = if b == 1 then " *" else "  "
		
------------------
-- insert function
------------------

-- insert Empty 
insert Empty lst
	| (length lst == 1) = Node (head lst) 1 []
	| (length lst > 1) = insert (Node (head lst) 0 []) (lst)
	| (length lst == 0) = Empty

-- insert into List dictionary
insert (List listOfDictionary) lst
	| ((length lst) == 0) = List listOfDictionary
	| ((length listOfDictionary) == 0) = List [insert Empty lst]
	| ((head lst) == (returnRoot(head listOfDictionary))) =  List ([insert (head listOfDictionary) lst] ++ (tail listOfDictionary))
	| ((head lst) < (returnRoot(head listOfDictionary))) =  List ( [insert Empty lst] ++ listOfDictionary)
	| ((head lst) > (returnRoot(head listOfDictionary))) =  List ( [(head listOfDictionary)] ++ [(insert (List (tail listOfDictionary)) lst)])
	
-- insert into Node dictionary	
insert (Node a b c) lst 
	| ((length lst) == 0) = Node a b c
	| ((head lst) == a) && (length lst == 1)  = changeSign (Node a b c)
	| ((head lst) == a) && (length lst > 1)  = (Node a b (insertIntoChildren c (tail lst)))
	|  (head lst) > a = List [(Node a b c), (insert Empty lst)] 
	|  otherwise = List [(insert Empty lst),(Node a b c)]

-- insert into children of a Node dictionary	
insertIntoChildren children eklenecek  
	| (length children) == 0 && (length eklenecek) == 1 = [Node (head eklenecek) 1 []]
	| (length children) == 0 && (length eklenecek) > 1 = [ (insert Empty eklenecek)] 
	| otherwise = insertIntoChildrenList children (eklenecek)

-- insert into List of children
insertIntoChildrenList children eklenecek
	| (length children) == 1 && (returnRoot (head children)) < (head eklenecek) = children ++ [ (insert Empty eklenecek)] 
	| (length children) == 1 && (returnRoot (head children)) > (head eklenecek) = [ (insert Empty eklenecek)] ++ children
	| foundInChildren children (head eklenecek) == False = insertIntoChildrenListNotFound children eklenecek
	| foundInChildren children (head eklenecek) == True = insertIntoChildrenListFound children eklenecek

-- insert into List of children if it is found in list
insertIntoChildrenListFound children eklenecek
	| (returnRoot (head children)) == (head eklenecek) = [ (insert (head children) eklenecek)]  ++ (tail children)
	| (returnRoot (head(tail children))) == (head eklenecek) = (init children) ++ [ (insert (head(tail children)) eklenecek)]
	| (returnRoot (head children)) < (head eklenecek) = [(head children)] ++ insertIntoChildrenListFound (tail children) eklenecek

-- insert into List of children if it is not found in list	
insertIntoChildrenListNotFound children eklenecek 
	| (returnRoot (head children)) > (head eklenecek) = [ (insert Empty eklenecek)] ++ children
	| (returnRoot (head(tail children))) < (head eklenecek) = children ++ [ (insert Empty eklenecek)]
	| (returnRoot (head children)) < (head eklenecek) = [(head children)] ++ insertIntoChildrenListNotFound (tail children) eklenecek

-- check whether or not it is found in children list	
foundInChildren c t 
	| length c == 0  = False
	| ((returnRoot (head c)) == t) = True
	| otherwise = foundInChildren (tail c) t

---------------------
-- fromList function
---------------------
fromList liste = foldl insert Empty liste

------------------
-- check function
------------------

-- check in List dictionary
check (List t) searched
	| (length t == 0) = False
	| (length searched == 0) = False 
	| ((head searched) < (returnRoot(head t))) =  False
	| ((head searched) == (returnRoot(head t))) =  check (head t) (searched)
	| ((head searched) > (returnRoot(head t))) =  check (List(tail t)) searched

-- check in Empty dictionary	
check Empty _ = False

-- check in Node dictionary
check (Node a b c) searched
	| (length searched == 0) = False
	| (head searched) == a && (length searched == 1) && (b==1) = True 
	| (head searched) /= a && (length searched == 1) = False 
	| (head searched) == a  = checkInChildren c (tail searched)
	| (head searched) /= a = False

-- check in children list 	
checkInChildren c searched
	| (length searched == 0) = False
	| (length c == 0) = False		
	| (returnRoot (head c)) > (head searched) = False
	| (returnRoot (head c)) == (head searched) = check (head c) searched
	| (returnRoot (head c)) < (head searched) = checkInChildren (tail c) searched

-------------------
-- toList function
-------------------

-- toList Empty dictionary
toList Empty = []	

-- toList List dictionary
toList (List t)
	| length t == 0 = []
	| otherwise = toList (head t) ++ toListChildren (tail t)
	
-- toList Node dictionary	
toList (Node a b c)
	| b == 1  = ([[a]] ++ if length c > 0 then toListChildren c else []) 
	| b == 0  = basaEkle a (if length c > 0 then toListChildren c else [])
	
-- toList list of children dictionary
toListChildren c 
	| length c == 0 =[]
	| otherwise = toList (head c) ++ toListChildren (tail c)

-- Helper function for adding to every element of list
basaEkle a b = map (a : ) b

-------------------
-- merge function
-------------------

-- merging two List dictionaries
merge (List liste1) (List liste2) = List (merge2children liste1 liste2)

-- merging List and Node dictionary
merge (List liste) (Node a1 b1 c1) 
	| returnRoot(last liste) < a1 = List (liste ++ [(Node a1 b1 c1)]) 
	| returnRoot(head liste) > a1 = List ([(Node a1 b1 c1)] ++ liste )
	| returnRoot(head liste) == a1 = List ([( merge2nodes (head liste) (Node a1 b1 c1))] ++ (tail liste))
	| returnRoot(head liste) < a1 = List ( [head liste] ++ [merge (Node a1 b1 c1)  (List (tail liste))])

-- merging Node and List dictionary
merge (Node a1 b1 c1) (List liste)
	| returnRoot(last liste) < a1 = List (liste ++ [(Node a1 b1 c1)]) 
	| returnRoot(head liste) > a1 = List ([(Node a1 b1 c1)] ++ liste )
	| returnRoot(head liste) == a1 = List ([( merge2nodes (head liste) (Node a1 b1 c1))] ++ (tail liste))
	| returnRoot(head liste) < a1 = List ( [head liste] ++ [merge (Node a1 b1 c1)  (List (tail liste))])
	
-- merging two Node dictionaries
merge (Node a1 b1 c1) (Node a2 b2 c2)
	| a1 > a2 = List [(Node a2 b2 c2),(Node a1 b1 c1)]
	| a2 > a1 = List [(Node a1 b1 c1),(Node a2 b2 c2)]
	| otherwise = merge2nodes  (Node a1 b1 c1) (Node a2 b2 c2)
	
-- Helper for merging two nodes
merge2nodes (Node a1 b1 c1) (Node a2 b2 c2)
	| b1 == 0 && b2 == 1 = (Node a1 1 (merge2children c1 c2))
	| b1 == 1 && b2 == 0 = (Node a1 1 (merge2children c1 c2))
	| otherwise  = (Node a1 b1 (merge2children c1 c2))

-- Helper for merging two children
merge2children c1 c2
	| length c1 == 0 = []
	| length c2 == 0 = []
	| returnRoot(last c1) < returnRoot(head c2) = c1 ++ c2 
	| returnRoot(head c1) > returnRoot(last c2) = c2 ++ c1 
	| returnRoot(head c1) > returnRoot(head c2) =  [(head c2)] ++ (merge2children (c1) (tail c2)) 
	| returnRoot(head c1) < returnRoot(head c2) = [(head c1)] ++ merge2children (tail c1) c2
	| returnRoot(head c1) == returnRoot(head c2) = [(merge2nodes (head c1) (head c2))] ++ merge2children (tail c1) (tail c2)

-------------------
-- subs function
-------------------
subs a b = fromList((toList a) Set.\\ (toList b))
-- Due to implementation errors ocurred, subs funtion is implemented without prefix tree

-------------------------------
-- Examples from homework text
-------------------------------
example1 = merge ( insert ( insert empty "tee") "into") (insert empty "tea")
example2 = toList example1
example3 = fromList ["inn","tee","tea","into","ted"]
example4 = subs (merge (insert (insert empty "tee") "into") (insert empty "tea")) (insert empty "tee")

-- End of code
-- April 24, 2012