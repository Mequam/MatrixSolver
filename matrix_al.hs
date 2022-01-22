--returns the first column of a matrix
col :: [[a]] -> [a]
col ((s1:s):arr) = s1 : (col arr)
col [] = []

--returns the first row of a matrix
row :: [[a]]->[a]
row (arr1:arr) = arr1
row [] = []

--return the given matrix without the first array
skim :: [[a]]->[[a]]
skim (arr1:arr) = arr

--gets a matrix without the first column
skimCol :: [[a]]->[[a]]
skimCol mat = [ arr | (v1:arr)<-mat]  

--gets the first column of a matrix
popCol :: [[a]]->[a]
popCol ((r1:row): mat) = r1 : (popCol mat)
popCol [] = []

--appends a column to the given matrix
appCol :: [[a]]->[a]->[[a]]
appCol ((m1):mat) (c1:col) = (c1 : m1) : (appCol mat col)
appCol [] [] = []
appCol mat [] = mat
appCol [] col = [col]

multRow :: (Fractional a) => a ->[a]->[a]
multRow a arr = [x*a|x<-arr]

addRow :: (Num a) => [a]->[a]->[a]
addRow (x1:r1) (x2:r2) = (x1+x2) : (addRow r1 r2)
addRow [] [] = []
addRow arr1 [] = arr1
addRow [] arr2 = arr2

addRowFuncMat :: (Num a) => ([a]->[a]->[a]) -> [a]->[[a]]->[[a]]
addRowFuncMat f row mat = [addRow (f row r) r | r <- mat]


addRowMat = addRowFuncMat (\x y -> x)

addScaledRowMat :: (Fractional a) => [a]->[[a]]->[[a]]
--takes an incoming row and scales it by the first value of a given row
addScaledRowMat = addRowFuncMat (\x (v:var) -> (multRow v x))

addNegScaledRowMat :: (Fractional a) => [a]->[[a]]->[[a]] 
addNegScaledRowMat = addRowFuncMat (\x (v:var) -> (multRow (-v) x)) 

propigateFirstRow (row1:mat) = row1 : (addScaledRowMat row1 mat)


getRowNFromS :: Integer -> [[a]] -> Integer ->[a]
getRowNFromS s (r:mat) n
 | s >= n = r
 | otherwise = (getRowNFromS (s+1) mat n)

getRowN = (getRowNFromS 0)


--unions the two arrays but flips the second one
unionArr :: [a]->[a]->[a]
unionArr (v1:arr1) arr2 = v1 : (unionArr arr1 arr2)
unionArr [] arr2 = arr2

appArr :: a->[a]->[a]
appArr v (x1:arr) = x1 : (appArr v arr)
appArr v [] = [v]

--takes the previous rows and the current matrix and solves them
solveMem :: (Fractional a,Eq a)=>[[a]] -> [[a]] -> [[a]]
solveMem matp ((x1:arr1):matc)
 | x1 == 1.0 = appCol (solveMem stripedTop stripedBot) stripedCol
 | otherwise = solveMem matp ((multRow (1 / x1) targetRow):matc)
 where
  targetRow = (x1:arr1)
 
  topRowAdded = appArr targetRow (addNegScaledRowMat targetRow matp) 
  bottomRowAdded = addNegScaledRowMat targetRow matc
 
  stripedTop = skimCol topRowAdded
  stripedBot = skimCol bottomRowAdded
  
  stripedCol = unionArr (popCol topRowAdded) (popCol bottomRowAdded)

solveMem matp [] = matp

solve = solveMem [] 


--string stuffs
stripVal :: (Eq a) => a -> [a] -> [a]
stripVal val (v1 : arr)
 | v1 == val = (stripVal val arr)
 | otherwise = v1 : (stripVal val arr)
stripVal _ [] = []

contains :: (Eq a) => [a] -> a->Bool
contains (v1:arr) val
 | val == v1 = True
 | otherwise = (contains arr val)
contains [] _ = False

subtractSet :: (Eq a) => [a] -> [a] -> [a]
subtractSet (s1:sub) arr = subtractSet sub (stripVal s1 arr)
subtractSet [] arr = arr

intersectSet :: (Eq a) => [a] -> [a] -> [a]
intersectSet arr1 (s2:arr2)
 | (contains arr1 s2) = s2 : (intersectSet arr1 arr2)
 | otherwise = intersectSet arr1 arr2
intersectSet _ [] = []

--maps all elements in an array within the given set to the given value
mappToVal :: (Eq a) => [a]-> a -> [a] -> [a]
mappToVal set val (x1:arr1)
 | (contains set x1) = val : (mappToVal set val arr1)
 | otherwise = x1 : (mappToVal set val arr1)
mappToVal _ _ [] = []

cleanString = (intersectSet "0123456789.+-=") . (subtractSet (' ':(['a'..'z'] `unionArr` ['A'..'Z'])))
