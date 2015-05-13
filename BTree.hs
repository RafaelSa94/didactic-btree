module BTree (M(..), BTree(..), insertB, removeB, newB, findB, inTreeB, readB) where
import Data.Monoid

--ALGEBRIC DATA TYPE
type M = Int
data BTree a = BTree M [a] [BTree a] deriving (Eq, Show, Read)
data ReadMode = ERD | RED | EDR deriving (Eq)

instance Functor BTree where
	fmap f (BTree m xs []) = BTree m (map f xs) []
	fmap f (BTree m xs cs) = BTree m (map f xs) (map (fmap f) cs)

--B-TREE FUNCTIONS
insertB :: (Ord a) => a -> BTree a -> BTree a
insertB i (BTree m [] []) = newB m [i] []
insertB i (BTree m (x:xs) [])
	| i `elem` (x:xs) = newB m (x:xs) []
	| i < x = if (tam < lim) then newB m (i:x:xs)  [] else quebraMeio (newB m (i:x:xs) [])
	| i > x = if (tam < lim) then newB m (x:novaD) [] else quebraMeio (newB m (x:novaD) [])
	where
		tam   = length (x:xs)
		lim   = 2*m 
		novaD = values (insertB i (newB m xs []))
insertB i (BTree 1 xs cs) = insertOrd1 i (BTree 1 xs cs)
insertB i (BTree m xs cs)
	| i == (head xs) = newB m xs cs
	| i < (head xs) = if (tamNovaE == 1) 
		then mergeB novaE (newB m xs (tail cs))
		else newB m xs ((insertB i (head cs)):(tail cs))
	| i > (head xs) = if (tamNovaD == 1) 
		then mergeB (newB m xs (take (tam-1) (cs))) novaD 
		else newB m xs (take (tam-1) (cs) ++ [(insertB i (last (cs)))])
	where
		newArv   = (take (tam-1) (cs))
		tam      = length cs
		novaE    = insertB i (head cs)		
		novaD    = insertB i (last (cs))
		tamNovaE = length $ values $ novaE
		tamNovaD = length $ values $ novaD

removeB :: (Ord a) => a -> BTree a -> BTree a
removeB r (BTree m xs cs) = insertBList (filter(/=r) $ readInOrd (BTree m xs cs)) (newB m [] [])

findB :: (Ord a) => a -> BTree a -> [a]
findB f (BTree m xs cs)
	| f > (maximum xs) = (last xs):(findB f (last cs)) 
	| f == (head xs) = [f]
	| f <  (head xs) = [head xs] ++ (findB f (head cs))
	| f >  (head xs) = findB f (newB m (tail xs) (tail cs))

readB :: ReadMode -> BTree a -> [a]
readB rm = case rm of 
	ERD -> readInOrd
	RED -> readPreOrd
	EDR -> readPosOrd

--AUXILIAR FUNCTIONS

newB = (\m v c -> BTree m v c)

insertOrd1 :: (Ord a) => a -> BTree a -> BTree a
insertOrd1 i (BTree 1 (x:xs) (c:cs))
	| i `elem` (x:xs) = newB 1 (x:xs) (c:cs)
	| i < x           = if sizeL < (length (values c))
		then mergeB newLeft (newB 1 (x:xs) cs)
		else newB 1 (x:xs) (newLeft:cs)
	| i > (last (x:xs))   = if sizeR < (length (values (last cs)))
		then mergeB (newB 1 (x:xs) (init (c:cs))) newRight
		else newB 1 (x:xs) ((init (c:cs)) ++ [newRight])
	| otherwise = mergeB (newB 1 [x] [c]) (insertB i $ newB 1 xs cs)
	where
		newLeft  = insertB i c
		newRight = insertB i (last cs)
		sizeL    = length $ values newLeft 
		sizeR    = length $ values newRight

mergeB :: (Ord a) => BTree a -> BTree a -> BTree a
mergeB (BTree m (x:xs) cs) (BTree _ [] []) = BTree m (x:xs) cs
mergeB (BTree m (x:xs) cs) (BTree _ (v:vs) ds) = if (tam < 2*m +1) 
	then novaArv 
	else quebraMeio novaArv
	where
		novaArv = newB m ((x:xs) ++ (v:vs)) (cs ++ ds)
		tam     = length $ values novaArv

quebraMeio :: (Ord a) => BTree a -> BTree a
quebraMeio (BTree m xs cs) = BTree m [xs!!m] [newB m esqV esqC, newB m dirV dirC]
	where
		esqV = take (m) xs
		esqC = take (m+1) cs
		dirV = drop (m+1) xs
		dirC = drop (m+1) cs

values :: BTree a -> [a]
values (BTree _ v _) = v

keys :: BTree a -> [BTree a]
keys (BTree _ _ cs) = cs

readPreOrd :: BTree a -> [a]
readPreOrd (BTree _ []     [])     = []
readPreOrd (BTree _ xs     [])     = xs
readPreOrd (BTree _ []     cs)     = readPreOrd (head cs) 
readPreOrd (BTree m (x:xs) (c:cs)) = (x:(readPreOrd c)) ++ (readPreOrd (newB m xs cs))

readInOrd :: BTree a -> [a]
readInOrd (BTree _ []     [])     = []
readInOrd (BTree _ xs     [])     = xs
readInOrd (BTree _ []     cs)     = readInOrd (head cs) 
readInOrd (BTree m (x:xs) (c:cs)) = ((readInOrd c) ++ [x]) ++ (readInOrd (newB m xs cs))

readPosOrd :: BTree a -> [a]
readPosOrd (BTree _ []     [])     = []
readPosOrd (BTree _ xs     [])     = xs
readPosOrd (BTree _ []     cs)     = readPosOrd (head cs) 
readPosOrd (BTree m (x:xs) (c:cs)) = if (length xs == length cs)
	then (readPosOrd c) ++ (readPosOrd (newB m xs cs)) 
	else (readPosOrd c) ++ (readPosOrd (head cs)) ++ (readPosOrd (newB m xs (tail cs))) ++ (x:xs)

inTreeB :: (Ord a) => a -> BTree a -> Bool
inTreeB f (BTree _ xs [])
	| f `elem` xs = True
	| otherwise   = False
inTreeB f (BTree m xs cs)
	| f `elem` xs = True
	| f > (maximum xs) = inTreeB f (last cs) 
	| f < (head xs)    = inTreeB f (head cs)
	| f > (head xs)    = inTreeB f (newB m (tail xs) (tail cs))

insertBList :: (Ord a) => [a] -> BTree a -> BTree a
insertBList []     (BTree m xs cs) = newB m xs cs
insertBList (x:xs) arv = insertBList xs $ insertB x arv
