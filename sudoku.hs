import Data.List
import System.IO  
import Control.Monad
System.IO.hSetNewlineMode

--puzzle :: [[Char]] -- puzzle-ul reprezinta o lista de stringuri (matrice de caractere)

-- puzzle = ["2....1.3.",
--           "........5",
--           ".7...6...",
--           ".......13",
--           ".981..257",
--           "31....8..",
--           "9..8...2.",
--           ".5..69784",
--           "4..25...."]

       
--checkRows [[1,2,3],[4,5,6],[7,8,9]] => [[1,2,3],[4,5,6],[7,8,9]]
checkRows :: [[a]] -> [[a]] 
checkRows m = m


-- checkColumns [[1,2,3],[4,5,6],[7,8,9]] => [[1,4,7],[2,5,8],[3,6,9]]
checkColumns :: [[a]] -> [[a]] 
checkColumns = transpose 

--checkBoxes [[1,2,3],[4,5,6],[7,8,9]] => [[1,2,3,4,5,6,7,8,9]]
checkBoxes :: [[a]] -> [[a]] 
checkBoxes = unpack . map checkColumns . pack
        where
          pack   = split . map split 
          split  = chop 3
          unpack = map concat . concat

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

--verificare sa nu existe duplicate intr-o lista
noDuplicates :: Eq a => [a] -> Bool
noDuplicates [] = True
noDuplicates (x:xs) = not (elem x xs) && noDuplicates xs

--verificare corectitudine
isValid :: [[Char]] -> Bool
isValid grid = all noDuplicates (checkRows grid) && all noDuplicates (checkColumns grid) && all noDuplicates (checkBoxes grid)
--un grid este isValid doar daca nu exista duplicate pe linii, coloane sau in cadrul unui patrat

--inlocuim fiecare spatiu gol cu toate numerele posibile
type PossibleValues = [Char] -- o lista de caractere ex. [1234] sau [12]

possibilities :: [[Char]] -> [[PossibleValues]] -- primeste un grid si returneaza o matrice de valori posibile
possibilities grid = map (map choice) grid
                        where 
                            choice val = if val == '.' then ['1'..'9'] -- daca e spatiu gol se pune o lista de valori
                                        else [val] --daca e o valoare se returneaza valoarea respectiva


-- cartesianProduct [[1,2,3],[4,5,6]] => [[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]
cartesianProduct :: [[a]] -> [[a]]
cartesianProduct [] = [[]]
cartesianProduct (xs : xss) = [y : ys | y <- xs, ys <- cartesianProduct xss]

--expandPossibilities [[[1],[2],[3]],[[1],[1,2,3],[2]]] => [[[1,2,3],[1,1,2]],[[1,2,3],[1,2,2]],[[1,2,3],[1,3,2]]]
expandPossibilities :: [[[a]]] -> [[[a]]] -- transforma o matrice de liste intr-o lista de matrici => extrage toate combinatiile de possibilities
expandPossibilities m = cartesianProduct(map cartesianProduct m)


filterP :: [PossibleValues] -> [PossibleValues]
filterP xss =  [xs `minus` singles | xs <- xss]
              where singles = concat (filter finalValue xss)
            
--elimina duplicatele de pe linii/coloane si patrate 
fliterPossibilities  :: [[PossibleValues]] -> [[PossibleValues]]
fliterPossibilities  = filterBy checkRows . filterBy checkColumns . filterBy checkBoxes
                        where filterBy  f =  f . map filterP .  f

--daca e o singura valoare se considera finala
finalValue :: [a] -> Bool
finalValue [_] = True
finalValue _  = False

--daca xs e o lista cu valori finale ramane asa, daca nu, scadem valorile gasite in ys
minus :: PossibleValues -> PossibleValues -> PossibleValues
xs `minus` ys = if finalValue xs then xs 
                else xs \\ ys

--aplicam functia filter pana cand solutia e finala (nu mai sunt valori in plus) 
repeatR :: Eq a => (a -> a) -> a -> a
repeatR f x = if x == x' then x else repeatR f x' 
    where x' = f x 

--Rezolvare
solution :: [[Char]] -> [[[Char]]] --primeste un grid si returneaza o lista de rezolvari posibile - in cazul corect e doar una 
solution grid = filter isValid (expandPossibilities (repeatR fliterPossibilities (possibilities grid)))


--Main
main :: IO ()
main = do
        handle <- openFile "input.txt" ReadMode --deschid fisierul
        contents <- hGetContents handle
        print contents
        let puzzle = lines contents --despart in linii, fiecare linie din input
        print puzzle -- printez puzzle-ul
        (putStrLn . unlines . head . solution) puzzle
        hClose handle   --inchid fisierul
