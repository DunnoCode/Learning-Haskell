{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFunctor #-}

module Transform
   ( Transform(..)
   , next
   , runTransform
   , evalTransform
   , Tree(..)
   , tFoldl
   , tToListWith
   ) where

import Control.Monad

newtype Transform a b = Transform { getTransform :: (b, a -> a) }
  deriving Functor


instance Applicative (Transform a) where
   pure = return
   (<*>) = liftM2 ($)

-- | Problem 2
instance Monad (Transform a) where
   -- return :: b -> Transform a b
   return b =  Transform (b, \a -> a) 

   -- (>>=) :: Transform a b -> (b -> Transform a c) -> Transform a c         Trans(c, a->a)
   -- (>>=) = undefined fst $ getTransform $ f $ fst $ getTransform m
   m >>= f = Transform (fst $ getTransform $ f $ fst $ getTransform m, (snd $ getTransform $ f $ fst $ getTransform m) . (snd $ getTransform $ m ))

next :: (a -> a) -> Transform a ()
next m = Transform((), m)

evalTransform :: Transform a b -> b
evalTransform = fst . getTransform

runTransform :: Transform a b -> a -> a
runTransform = snd . getTransform

countedFibonacci :: Int -> Transform Int Int
countedFibonacci 0 = return 0
countedFibonacci 1 = return 1
countedFibonacci n = do
   a <- countedFibonacci (n - 1)
   next (+1)
   b <- countedFibonacci (n - 2)
   return $ a + b

data Tree a = Leaf | Branch (Tree a) a (Tree a)


-- | Problem 3
next2 :: ([a] -> [a]) -> Transform [a] ()
next2 m = Transform((), m)

tFoldl :: (b -> a -> b) -> b -> Tree a -> Transform [a] b
tFoldl f string Leaf = return string
tFoldl f string (Branch Leaf x Leaf) = do 
                           next2(++[x])
                           return (f string x)
tFoldl f string (Branch i j k) = do
   a <- tFoldl f string i
   b <- tFoldl f a (Branch Leaf j Leaf)
   c <- tFoldl f b k
   return $ c
   -- a <- tFoldl f (fst $ getTransform $ tFoldl f string (Branch Leaf j Leaf)) i
-- tFoldl f string (Branch i j k) = do
--    a <- tFoldl f string i
--    b <- tFoldl f string (Branch Leaf j Leaf)
--    b <- tFoldl f string k 
--    return $ b


--    next ()
--    return (f string j)
-- tFoldl f string (Branch i j k) = return (f (fst $ getTransform $ tFoldl f string i) j)
-- tFoldl f string (Branch i j k) = return (tFoldl f (tFoldl f (f string j) i ) k) 

-- (Branch Leaf 1 Leaf)
-- evalTransform (tFoldl (\b x -> b ++ show x) "" t)
-- (\b x -> b ++ show x) :: Show a => [Char] -> a -> [Char]
-- getTransform :: Transform a b -> (b, a -> a)
-- Transform :: (b, a -> a) -> Transform a b
next3 :: (b -> b) -> Transform b ()
next3 m = Transform ((), m)

tToListWith :: (b -> a -> b) -> Tree a -> Transform b [a]
tToListWith f Leaf = return []
tToListWith f (Branch Leaf x Leaf) = do 
                     next3 ((flip f) x)
                     return [x]
tToListWith f (Branch a b c) = do
   x <- tToListWith f a
   y <- tToListWith f (Branch Leaf b Leaf)
   z <- tToListWith f c
   return $ x ++ y ++ z
-- t = Branch (Branch (Branch Leaf 1 Leaf) 2 (Branch Leaf 3 Leaf)) 4 (Branch (Branch Leaf 5 Leaf) 6 Leaf)
-- (\b x -> b ++ show x)


