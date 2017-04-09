---
title: Hanoi with Binary and Haskell
---

A while ago I found a video describing how to solve towers of hanoi using binary.

https://www.youtube.com/watch?v=2SUvWfNJSsM&t=398s

```haskell

type ResultLog a = Writer [a] a

record :: a -> Writer [a] a
record = pass . pure . (id &&& (:))

data Zipper a = Z [a] a [a] deriving (Show)

toZipper (x:xs) = Z [] x xs

zipLeft (Z (l:ls) m []) = Z [] l (ls ++ [m])
zipLeft (Z l m (r:rs)) = Z (l ++ [m]) r rs

insertToMid :: a -> Zipper [a] -> [[a]]
insertToMid m (Z l ms r) = l ++ [(m:ms)] ++ r 

canMoveRight (Z l (m:ms) ([]:r)) = True
canMoveRight (Z l (m:ms) ((n:r):rs)) = m < n
canMoveRight (Z ([]:ls) (m:ms) []) = True
canMoveRight (Z ((n:l):ls) (m:ms) []) = m < n
canMoveRight _ = False

hanoiMove :: Int -> Zipper [Int] -> [[Int]]
hanoiMove 0 (Z l m r) = l ++ [m] ++ r
hanoiMove d z@(Z l [] r) = hanoiMove d (zipLeft z)
hanoiMove d z@(Z l (m:ms) r)
  | d == m && canMoveRight z = insertToMid m $ zipLeft (Z l ms r)
  | d == m = insertToMid m . zipLeft . zipLeft $ (Z l ms r)
  | otherwise = hanoiMove d (zipLeft z)

moves ds = fmap (hanoiMove . (!!) ds . countTrailingZeros) ns
  where ns = concat . take 2 $ repeat ([1..(2^(length ds)-1)]::[Int])

hanoi :: [Int] -> ResultLog [[Int]]
hanoi ds = foldM (\b f -> record (f (toZipper b))) startingBoard  (moves ds)
  where startingBoard = ds:(take (length ds-1) $ repeat [])
  
```
