module Combinators
  ( lookAhead
  , optional
  , choose
  , followedBy
  , manyUntil) where

import Parser

import Control.Applicative hiding (optional)
import Control.Monad ()
import Control.Monad.State

import Control.Arrow

import Data.Char
import Data.Functor
import Data.Monoid

{- * Question 1 - 5. (35 pts)

Replace `undefined` with your implementation.
You are free to add auxilliary functions or data types to aid your
implementation.
-}

lookAhead :: Parser Char
lookAhead = parser (\s ->
  case s of 
    "" -> []
    _ -> [(head s, s)])

optional :: Parser a -> Parser (Maybe a)
optional p = parser (\s ->
  case (runParser p s) of
    [] -> [(Nothing, s)]
    [(x, [])] -> [(Just x, s)])

choose :: [Parser a] -> Parser a
choose ps = parser (\s -> choose1 ps s)

choose1 :: [Parser a] -> String -> [(a, String)]
choose1 list input
  | return0 = []
  | otherwise = (runParser (head list) input) ++ (choose1 (tail list) input)
  where
    return0 = length list == 0

followedBy :: Parser a -> Parser b -> Parser a
followedBy a b = parser (\s -> followedBy1 a b s)

followedBy1 :: Parser a -> Parser b -> String -> [(a, String)]
followedBy1 a b s = case (runParser a s) of
                      [] -> []
                      [(_, s1)] -> case (runParser b s1) of
                                    [] -> []
                                    _  -> runParser a s


manyUntil :: Parser a -> Parser b -> Parser [a]
manyUntil a b = parser (\s -> output a b s 0)

output :: Parser a -> Parser b -> String -> Int -> [([a], String)]
output a b s index
  | return0 = []
  | otherwise = case (runParser b (drop index s)) of
                  [] -> [] ++ output a b s (index + 1)
                  [(x, s1)] -> if ((check a (take index s)) == False) 
                                  then [] ++ output a b s (index + 1)
                                  else [((makeList a b s s1 index), s1)] ++ output a b s (length s - (length s1))
                   
  where
    return0 = (length s < index) == True

check :: Parser a -> String -> Bool
check a s
  |return0 = True
  |otherwise = case (runParser a s) of
                [] -> False
                [(x, s1)] -> check a s1
  where
    return0 = s == "" 

makeList :: Parser a -> Parser b -> String -> String -> Int -> [a]
makeList a b input input2 index = case (runParser (many1 a) (take index input)) of 
                            [] -> []
                            (list, remain):xs -> list

                                      
         