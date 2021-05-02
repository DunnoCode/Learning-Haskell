{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFunctor #-}

module ListMaybe
  ( LM(..)
  , ML(..)
  ) where
import Control.Applicative
import Control.Monad

import Data.Maybe
import Data.List

newtype LM a = LM { getLM :: [Maybe a] }
  deriving Functor

instance Applicative LM where
  pure = return
  (<*>) = liftM2 ($)

-- Seems wrong :(
-- | Problem 4.1
walkthrough :: [Maybe a] -> (a -> LM b) -> [Maybe b]
walkthrough (x:xs) function 
  | length xs == 0 = case x of
                        Nothing -> [Nothing]
                        Just a  -> getLM (function a)
  | otherwise = case x of 
                    Nothing -> [Nothing] ++ walkthrough xs function
                    Just a -> getLM (function a) ++ walkthrough xs function

instance Monad LM where
  return :: a -> LM a
  return a = LM ([Just a])

  (>>=) :: LM a -> (a -> LM b) -> LM b
  m >>= f = case m of
                LM [] -> LM []
                LM list -> LM (walkthrough list f)

  -- LM [Nothing] >>= f = LM [Nothing] 
  -- LM [Just x] >>= f = f x
  -- (>>=)  :: [a] -> (a -> [b]) -> [b]
  --  xs >>= f = concatMap f xs

-- [] -> []
-- Nothing : as -> Nothing : fmap f as
-- Just a : as -> f a ++ fmap f as

newtype ML a = ML { getML :: Maybe [a] }
  deriving Functor

instance Applicative ML where
  pure = return
  (<*>) = liftM2 ($)

-- | Problem 4.2
walkthrough2 :: [a] -> (a -> ML b) -> [b]
walkthrough2 (x:xs) function 
    | length xs == 0 = case getML (function x) of 
                            Just [a] -> [a]
    | otherwise = case getML (function x) of
                            Nothing -> walkthrough2 xs function
                            Just [a] -> [a] ++ walkthrough2 xs function

instance Monad ML where
  return :: a -> ML a
  return a = ML (Just [a])

  (>>=) :: ML a -> (a -> ML b) -> ML b
  m >>= f = case m of 
                ML (Nothing) -> ML (Nothing)
                ML (Just []) -> ML (Just [])
                ML (Just list) -> ML (Just (walkthrough2 list f))
 


  -- m >>= g = case m of 
  --             Nothing -> Nothing []  

  --             Just [a]  -> g [a]

