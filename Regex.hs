module Regex
  ( Regex
  , regexP
  , matcher
  ) where

import Combinators
import Parser

import Control.Applicative hiding (optional)
import Control.Arrow
import Control.Monad
import Control.Monad.State
import Prelude

import Data.Char
import Data.Functor
import Data.List
import Data.Monoid

import Text.Printf

{- * Question 6 - 7. (40 pts)

Add definitions of Regex, replace `undefined` with your implementation.
You are encouraged to add auxilliary functions and data types to aid your
implementation.

-}


data Regex = Empty                  -- empty
           | Word Char              -- {a}
           | Dot                    -- .
           | Star Regex             -- r*
           | Plus Regex             -- r+
           | Question Regex         -- r1? 
           | OR Regex Regex         -- r1 | r2
           | Concat Regex Regex     -- r1 r2 --> r1r2
           | Parentheses Regex      -- (r)
     deriving (Eq)

showRegex :: Regex -> String
showRegex (Word c)               = [c]
showRegex (Star r)               = showRegex r ++ "*"
showRegex (Plus r)               = showRegex r ++ "+"
showRegex (Concat r1 r2)         = showRegex r1 ++ showRegex r2
showRegex (OR r1 r2)             = showRegex r1 ++  "|" ++ showRegex r2
showRegex (Question r)           = showRegex r ++ "?"
showRegex (Dot)                  = "." 
showRegex (Parentheses r)        = "(" ++ showRegex r ++ ")"


instance Show Regex where
  -- show :: Regex -> String
  show r = showRegex r

  -- Alternatively, you can define `showsPrec` instead of `show`,
  -- as long as you know what you are doing.
  -- -- showsPrec :: Int -> Regex -> String
  -- showsPrec = undefined

{-
  Check whether it is a correct regex input before remove parentheses
  False will return an empty list 
  True will remove the cases in cleaner before entering the create funciton
-}
regexP :: Parser Regex
regexP = parser (\s -> 
            case (check (head s) 0 0 (tail s)) of
              True -> (create (head (cleaner s [])) 0 [] [] (tail (cleaner s [])))
              False -> [])

--remove the case which (a)* or (a)+ only one character inside a parentheses 
cleaner :: String -> String -> String
cleaner input output
   | return    = output
   | cut       = cleaner (tail(tail(tail (tail input)))) (output ++ [(head (tail input))] ++ [(head(tail(tail (tail input))))])
   | otherwise = cleaner (tail input) (output ++ [(head input)])
   where
        return = (length input == 0)
        cut = (head input == '(') && (head(tail (tail input)) == ')') && (head(tail(tail(tail input))) == '+' || head(tail(tail(tail input))) == '?' || head(tail(tail(tail input))) == '*')

{-
  two vectors is used for create funciton to perform a similar funciton of stunting yard 
  one is to store the two operates ( | and the regex list to store the word
  Example (r1|r2) operator = "(|" and regex list will be [r1,r2]
  https://medium.com/@gregorycernera/converting-regular-expressions-to-postfix-notation-with-the-shunting-yard-algorithm-63d22ea1cf88
  Turing the string input into Regex type and checking need to add an regex called parentheses in order to print out ()
  when using show
-}    
create :: Char -> Int -> [Char] -> [Regex] -> String -> [(Regex, String)]
create previous start opearte regex_list input_string
  | return0 = case (previous) of  -- first step of the recusion 
                '(' -> create (previous) 1 ['('] regex_list input_string 
                '.' -> create (previous) 1 opearte (regex_list ++ [Dot]) input_string
                _ -> create (previous) 1 opearte [(Word (previous))] input_string
  | return1 = case (previous) of --upcoming step for the recusion
                '|' -> case (head input_string) of
                          '.' -> case (length opearte == 1) of 
                                  True -> create (head input_string) 1 (init opearte) ((init regex_list) ++ [OR (last regex_list) (Dot)]) (tail input_string)
                                  False -> case (last (init opearte) == '|') of
                                                True -> create (head input_string) 1 (init opearte) ((init regex_list) ++ [OR (last regex_list) (Dot)]) (tail input_string)
                                                False -> create (head input_string) 1 opearte (regex_list ++ [Dot]) (tail input_string)
                          '(' -> create '(' 1 (opearte ++ ['(']) regex_list (tail input_string)
                          _ -> case (length opearte == 1) of 
                                  True -> create (head input_string) 1 (init opearte) ((init regex_list) ++ [OR (last regex_list) (Word (head input_string))]) (tail input_string)
                                  False -> case (last (init opearte) == '|') of
                                                True -> create (head input_string) 1 (init opearte) ((init regex_list) ++ [OR (last regex_list) (Word (head input_string))]) (tail input_string)
                                                False -> create (head input_string) 1 opearte (regex_list ++ [Word (head input_string)]) (tail input_string)
                          -- _ -> case (last opearte == '(') of 
                          --         True -> create (head input_string) 1 (opearte) ((regex_list) ++ [(Word (head input_string))]) (tail input_string)
                          --         False -> create (head input_string) 1 (init opearte) ((init regex_list) ++ [OR (last regex_list) (Word (head input_string))]) (tail input_string)
                '(' -> case (head input_string) of
                          '.' -> create '.' 1 opearte (regex_list ++ [Dot]) (tail input_string)
                          ')' -> create ')' 1 (init opearte) (regex_list) (tail input_string)
                          '(' -> create '(' 1 (opearte ++ ['(']) regex_list (tail input_string)
                          _ -> create (head input_string) 1 opearte (regex_list ++ [Word (head input_string)]) (tail input_string)
                -- + - ? are the same
                '*' -> case (head input_string) of
                          '.' -> create '.' 1 opearte (init(regex_list) ++ [Concat (last regex_list) (Dot)]) (tail input_string)
                          '(' -> create '(' 1 (opearte ++ ['(']) regex_list (tail input_string)
                          '|' -> create '|' 1 (opearte ++ [(head input_string)]) regex_list (tail input_string) 
                          ')' -> case (last opearte) of
                                    '|' -> case (length regex_list == 2) of 
                                            True -> case (tail input_string == "") of --(r1|r2)
                                                        True -> create ')' 1 (init(init opearte)) (init(init regex_list) ++ [OR (last(init regex_list)) (last regex_list)]) (tail input_string)
                                                        False -> case (head (tail input_string)) of --(r1|r2)*
                                                                      '*' -> create ')' 1 (init (init opearte)) (init(init regex_list) ++ [Parentheses (OR (last(init regex_list)) (last regex_list))]) (tail input_string)
                                                                      '+' -> create ')' 1 (init (init opearte)) (init(init regex_list) ++ [Parentheses (OR (last(init regex_list)) (last regex_list))]) (tail input_string)
                                                                      '?' -> create ')' 1 (init (init opearte)) (init(init regex_list) ++ [Parentheses (OR (last(init regex_list)) (last regex_list))]) (tail input_string)
                                                                      ')' -> create ')' 1 (init (init opearte)) (init(init regex_list) ++ [Parentheses (OR (last(init regex_list)) (last regex_list))]) (tail input_string)
                                                                      '(' -> create ')' 1 (init (init opearte)) (init(init regex_list) ++ [Parentheses (OR (last(init regex_list)) (last regex_list))]) (tail input_string)
                                                                      '|' -> create ')' 1 (init(init opearte)) (init(init regex_list) ++ [OR (last(init regex_list)) (last regex_list)]) (tail input_string)
                                                                      '.' -> create ')' 1 (init (init opearte)) (init(init regex_list) ++ [Parentheses (OR (last(init regex_list)) (last regex_list))]) (tail input_string)
                                                                      _ -> create ')' 1 (init (init opearte)) (init(init regex_list) ++ [Parentheses (OR (last(init regex_list)) (last regex_list))]) (tail input_string)
                                            False -> case (tail input_string == "") of -- True -> r1(r2|r3) 
                                                        True -> case ((length opearte) == 2) of -- True r1(r2|r3) --False r1|(r2|r3) or (r1(r2|r3)) 
                                                                True -> create ')' 1 (init(init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                False -> case(last(init(init opearte))) of
                                                                           '|' -> create ')' 1 (init(init(init opearte))) (init(init(init regex_list)) ++ [OR (last(init(init regex_list))) (OR (last(init regex_list)) (last regex_list))]) (tail input_string)
                                                                           '(' -> create ')' 1 (init(init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                        False -> case ((length opearte) == 2) of -- True -> r1(r2|r3)* False -> (r1|(r2|r3)) or r1|(r2|r3)*
                                                                    True -> case (head(tail input_string)) of
                                                                              '*' -> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                              '+' -> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                              '?' -> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                              '('-> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (OR (last(init regex_list)) (last regex_list))]) (tail input_string)
                                                                              ')'-> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                              '|'-> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (OR (last(init regex_list)) (last regex_list))]) (tail input_string)
                                                                              '.' -> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                              _ -> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                    False -> case (last(init(init opearte)) == '|') of --True -> (r1|(r2|r3))
                                                                            True -> case (head(tail input_string)) of
                                                                                '*' -> create ')' 1 (init(init(init opearte))) (init(init(init regex_list)) ++ [OR (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                                '+' -> create ')' 1 (init(init (init opearte))) (init(init(init regex_list)) ++ [OR (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                                '?' -> create ')' 1 (init(init (init opearte))) (init(init(init regex_list)) ++ [OR (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                                '|' -> create ')' 1 (init(init(init opearte))) (init(init regex_list) ++ [OR (last(init(init regex_list))) (OR (last(init regex_list)) (last regex_list))]) (tail input_string)   
                                                                                '(' -> create ')' 1 (init(init (init opearte))) (init(init(init regex_list)) ++ [OR (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                                ')' -> create ')' 1 (init(init (init opearte))) (init(init(init regex_list)) ++ [OR (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)   
                                                                                '.' -> create ')' 1 (init(init (init opearte))) (init(init(init regex_list)) ++ [OR (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                                _ -> create ')' 1 (init(init (init opearte))) (init(init(init regex_list)) ++ [OR (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                            False -> case (head(tail input_string)) of
                                                                                '*' -> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                                '+' -> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                                '?' -> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                                '(' -> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                                ')' -> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                                '|'-> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (OR (last(init regex_list)) (last regex_list))]) (tail input_string)
                                                                                '.' -> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                                _ -> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                                
                                    '(' -> case (length regex_list == 1) of
                                              True -> case (tail input_string == "") of  --(r)
                                                          True -> create ')' 1 (init opearte) regex_list (tail input_string)
                                                          False -> case (head (tail input_string)) of 
                                                                      '*' -> create ')' 1 (init opearte) (init regex_list ++ [Parentheses (last regex_list)]) (tail input_string)
                                                                      '+' -> create ')' 1 (init opearte) (init regex_list ++ [Parentheses (last regex_list)]) (tail input_string)
                                                                      '?' -> create ')' 1 (init opearte) (init regex_list ++ [Parentheses (last regex_list)]) (tail input_string)
                                                                      _ -> create ')' 1 (init opearte) regex_list (tail input_string)
                                              False -> case (tail input_string == "") of --r|()
                                                          True -> case (init opearte == "|") of 
                                                                    True ->  create ')' 1 (init(init opearte)) (init(init regex_list) ++ [OR (last(init regex_list)) (last regex_list)]) (tail input_string)
                                                                    False -> create ')' 1 (init opearte) (init(init regex_list) ++ [Concat (last(init regex_list)) (last regex_list)]) (tail input_string)
                                                          False -> case (init opearte == "") of --r(r) -> True r|(r)--> False
                                                                      True -> case (head(tail input_string)) of
                                                                              --need to separe to case r(r) r|(r) (a(b)*)
                                                                              '*' -> create ')' 1 (init opearte) (init(init regex_list) ++ [Concat (last(init regex_list)) (Parentheses (last regex_list))]) (tail input_string)
                                                                              '+' -> create ')' 1 (init opearte) (init(init regex_list) ++ [Concat (last(init regex_list)) (Parentheses (last regex_list))]) (tail input_string)
                                                                              '?' -> create ')' 1 (init opearte) (init(init regex_list) ++ [Concat (last(init regex_list)) (Parentheses (last regex_list))]) (tail input_string)
                                                                              _ -> create ')' 1 (init opearte) (init(init regex_list) ++ [Concat (last(init regex_list)) (last regex_list)]) (tail input_string)
                                                                      False -> case (last(init opearte)) of
                                                                                  '|' -> case (head(tail input_string)) of
                                                                                          '*' -> create ')' 1 (init (init opearte)) (init(init regex_list) ++ [OR (last(init regex_list)) (Parentheses (last regex_list))]) (tail input_string)
                                                                                          '+' -> create ')' 1 (init (init opearte)) (init(init regex_list) ++ [OR (last(init regex_list)) (Parentheses (last regex_list))]) (tail input_string)
                                                                                          '?' -> create ')' 1 (init (init opearte)) (init(init regex_list) ++ [OR (last(init regex_list)) (Parentheses (last regex_list))]) (tail input_string)
                                                                                          _ -> create ')' 1 (init (init opearte)) (init(init regex_list) ++ [OR (last(init regex_list)) (last regex_list)]) (tail input_string)
                                                                                  '(' -> case (head(tail input_string)) of
                                                                                          '*' -> create ')' 1 (init opearte) (init(init regex_list) ++ [Concat (last(init regex_list)) (Parentheses (last regex_list))]) (tail input_string)
                                                                                          '+' -> create ')' 1 (init opearte) (init(init regex_list) ++ [Concat (last(init regex_list)) (Parentheses (last regex_list))]) (tail input_string)
                                                                                          '?' -> create ')' 1 (init opearte) (init(init regex_list) ++ [Concat (last(init regex_list)) (Parentheses (last regex_list))]) (tail input_string)
                                                                                          _ -> create ')' 1 (init opearte) (init(init regex_list) ++ [Concat (last(init regex_list)) (last regex_list)]) (tail input_string)
                          _ -> create (head input_string) 1 opearte (init (regex_list) ++ [Concat (last regex_list) (Word (head input_string))]) (tail input_string)       
                '?' -> case (head input_string) of
                          '.' -> create '.' 1 opearte (init(regex_list) ++ [Concat (last regex_list) (Dot)]) (tail input_string)
                          '(' -> create '(' 1 (opearte ++ ['(']) regex_list (tail input_string)
                          '|' -> create '|' 1 (opearte ++ [(head input_string)]) regex_list (tail input_string) 
                          ')' -> case (last opearte) of
                                    '|' -> case (length regex_list == 2) of 
                                            True -> case (tail input_string == "") of --(r1|r2)
                                                        True -> create ')' 1 (init(init opearte)) (init(init regex_list) ++ [OR (last(init regex_list)) (last regex_list)]) (tail input_string)
                                                        False -> case (head (tail input_string)) of --(r1|r2)*
                                                                      '*' -> create ')' 1 (init (init opearte)) (init(init regex_list) ++ [Parentheses (OR (last(init regex_list)) (last regex_list))]) (tail input_string)
                                                                      '+' -> create ')' 1 (init (init opearte)) (init(init regex_list) ++ [Parentheses (OR (last(init regex_list)) (last regex_list))]) (tail input_string)
                                                                      '?' -> create ')' 1 (init (init opearte)) (init(init regex_list) ++ [Parentheses (OR (last(init regex_list)) (last regex_list))]) (tail input_string)
                                                                      ')' -> create ')' 1 (init (init opearte)) (init(init regex_list) ++ [Parentheses (OR (last(init regex_list)) (last regex_list))]) (tail input_string)
                                                                      '(' -> create ')' 1 (init (init opearte)) (init(init regex_list) ++ [Parentheses (OR (last(init regex_list)) (last regex_list))]) (tail input_string)
                                                                      '|' -> create ')' 1 (init(init opearte)) (init(init regex_list) ++ [OR (last(init regex_list)) (last regex_list)]) (tail input_string)
                                                                      '.' -> create ')' 1 (init (init opearte)) (init(init regex_list) ++ [Parentheses (OR (last(init regex_list)) (last regex_list))]) (tail input_string)
                                                                      _ -> create ')' 1 (init (init opearte)) (init(init regex_list) ++ [Parentheses (OR (last(init regex_list)) (last regex_list))]) (tail input_string)
                                            False -> case (tail input_string == "") of -- True -> r1(r2|r3) 
                                                        True -> case ((length opearte) == 2) of -- True r1(r2|r3) --False r1|(r2|r3) or (r1(r2|r3)) 
                                                                True -> create ')' 1 (init(init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                False -> case(last(init(init opearte))) of
                                                                           '|' -> create ')' 1 (init(init(init opearte))) (init(init(init regex_list)) ++ [OR (last(init(init regex_list))) (OR (last(init regex_list)) (last regex_list))]) (tail input_string)
                                                                           '(' -> create ')' 1 (init(init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                        False -> case ((length opearte) == 2) of -- True -> r1(r2|r3)* False -> (r1|(r2|r3)) or r1|(r2|r3)*
                                                                    True -> case (head(tail input_string)) of
                                                                              '*' -> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                              '+' -> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                              '?' -> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                              '('-> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (OR (last(init regex_list)) (last regex_list))]) (tail input_string)
                                                                              ')'-> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                              '|'-> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (OR (last(init regex_list)) (last regex_list))]) (tail input_string)
                                                                              '.' -> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                              _ -> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                    False -> case (last(init(init opearte)) == '|') of --True -> (r1|(r2|r3))
                                                                            True -> case (head(tail input_string)) of
                                                                                '*' -> create ')' 1 (init(init(init opearte))) (init(init(init regex_list)) ++ [OR (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                                '+' -> create ')' 1 (init(init (init opearte))) (init(init(init regex_list)) ++ [OR (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                                '?' -> create ')' 1 (init(init (init opearte))) (init(init(init regex_list)) ++ [OR (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                                '|' -> create ')' 1 (init(init(init opearte))) (init(init regex_list) ++ [OR (last(init(init regex_list))) (OR (last(init regex_list)) (last regex_list))]) (tail input_string)   
                                                                                '(' -> create ')' 1 (init(init (init opearte))) (init(init(init regex_list)) ++ [OR (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                                ')' -> create ')' 1 (init(init (init opearte))) (init(init(init regex_list)) ++ [OR (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)   
                                                                                '.' -> create ')' 1 (init(init (init opearte))) (init(init(init regex_list)) ++ [OR (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                                _ -> create ')' 1 (init(init (init opearte))) (init(init(init regex_list)) ++ [OR (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                            False -> case (head(tail input_string)) of
                                                                                '*' -> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                                '+' -> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                                '?' -> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                                '(' -> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                                ')' -> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                                '|'-> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (OR (last(init regex_list)) (last regex_list))]) (tail input_string)
                                                                                '.' -> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                                _ -> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                                
                                    '(' -> case (length regex_list == 1) of
                                              True -> case (tail input_string == "") of  --(r)
                                                          True -> create ')' 1 (init opearte) regex_list (tail input_string)
                                                          False -> case (head (tail input_string)) of 
                                                                      '*' -> create ')' 1 (init opearte) (init regex_list ++ [Parentheses (last regex_list)]) (tail input_string)
                                                                      '+' -> create ')' 1 (init opearte) (init regex_list ++ [Parentheses (last regex_list)]) (tail input_string)
                                                                      '?' -> create ')' 1 (init opearte) (init regex_list ++ [Parentheses (last regex_list)]) (tail input_string)
                                                                      _ -> create ')' 1 (init opearte) regex_list (tail input_string)
                                              False -> case (tail input_string == "") of --r|()
                                                          True -> case (init opearte == "|") of 
                                                                    True ->  create ')' 1 (init(init opearte)) (init(init regex_list) ++ [OR (last(init regex_list)) (last regex_list)]) (tail input_string)
                                                                    False -> create ')' 1 (init opearte) (init(init regex_list) ++ [Concat (last(init regex_list)) (last regex_list)]) (tail input_string)
                                                          False -> case (init opearte == "") of --r(r) -> True r|(r)--> False
                                                                      True -> case (head(tail input_string)) of
                                                                              --need to separe to case r(r) r|(r) (a(b)*)
                                                                              '*' -> create ')' 1 (init opearte) (init(init regex_list) ++ [Concat (last(init regex_list)) (Parentheses (last regex_list))]) (tail input_string)
                                                                              '+' -> create ')' 1 (init opearte) (init(init regex_list) ++ [Concat (last(init regex_list)) (Parentheses (last regex_list))]) (tail input_string)
                                                                              '?' -> create ')' 1 (init opearte) (init(init regex_list) ++ [Concat (last(init regex_list)) (Parentheses (last regex_list))]) (tail input_string)
                                                                              _ -> create ')' 1 (init opearte) (init(init regex_list) ++ [Concat (last(init regex_list)) (last regex_list)]) (tail input_string)
                                                                      False -> case (last(init opearte)) of
                                                                                  '|' -> case (head(tail input_string)) of
                                                                                          '*' -> create ')' 1 (init (init opearte)) (init(init regex_list) ++ [OR (last(init regex_list)) (Parentheses (last regex_list))]) (tail input_string)
                                                                                          '+' -> create ')' 1 (init (init opearte)) (init(init regex_list) ++ [OR (last(init regex_list)) (Parentheses (last regex_list))]) (tail input_string)
                                                                                          '?' -> create ')' 1 (init (init opearte)) (init(init regex_list) ++ [OR (last(init regex_list)) (Parentheses (last regex_list))]) (tail input_string)
                                                                                          _ -> create ')' 1 (init (init opearte)) (init(init regex_list) ++ [OR (last(init regex_list)) (last regex_list)]) (tail input_string)
                                                                                  '(' -> case (head(tail input_string)) of
                                                                                          '*' -> create ')' 1 (init opearte) (init(init regex_list) ++ [Concat (last(init regex_list)) (Parentheses (last regex_list))]) (tail input_string)
                                                                                          '+' -> create ')' 1 (init opearte) (init(init regex_list) ++ [Concat (last(init regex_list)) (Parentheses (last regex_list))]) (tail input_string)
                                                                                          '?' -> create ')' 1 (init opearte) (init(init regex_list) ++ [Concat (last(init regex_list)) (Parentheses (last regex_list))]) (tail input_string)
                                                                                          _ -> create ')' 1 (init opearte) (init(init regex_list) ++ [Concat (last(init regex_list)) (last regex_list)]) (tail input_string)
                          _ -> create (head input_string) 1 opearte (init (regex_list) ++ [Concat (last regex_list) (Word (head input_string))]) (tail input_string)   
                ')' -> case (head input_string) of -- all case ok * _ ? . + ( ) |
                          '*' -> create '*' 1 opearte (init(regex_list) ++ [Star (last regex_list)]) (tail input_string)
                          '?' -> create '?' 1 opearte (init(regex_list) ++ [Question (last regex_list)]) (tail input_string)
                          '+' -> create '+' 1 opearte (init(regex_list) ++ [Plus (last regex_list)]) (tail input_string)
                          '.' -> create '.' 1 opearte (init(regex_list) ++ [Concat (last regex_list) (Dot)]) (tail input_string)
                          '|' -> create '|' 1 (opearte ++ [(head input_string)]) regex_list (tail input_string) 
                          ')' -> case (last opearte) of
                                    '|' -> case (length regex_list == 2) of 
                                            True -> case (tail input_string == "") of --(r1|r2)
                                                        True -> create ')' 1 (init(init opearte)) (init(init regex_list) ++ [OR (last(init regex_list)) (last regex_list)]) (tail input_string)
                                                        False -> case (head (tail input_string)) of --(r1|r2)*
                                                                      '*' -> create ')' 1 (init (init opearte)) (init(init regex_list) ++ [Parentheses (OR (last(init regex_list)) (last regex_list))]) (tail input_string)
                                                                      '+' -> create ')' 1 (init (init opearte)) (init(init regex_list) ++ [Parentheses (OR (last(init regex_list)) (last regex_list))]) (tail input_string)
                                                                      '?' -> create ')' 1 (init (init opearte)) (init(init regex_list) ++ [Parentheses (OR (last(init regex_list)) (last regex_list))]) (tail input_string)
                                                                      ')' -> create ')' 1 (init (init opearte)) (init(init regex_list) ++ [Parentheses (OR (last(init regex_list)) (last regex_list))]) (tail input_string)
                                                                      '(' -> create ')' 1 (init (init opearte)) (init(init regex_list) ++ [Parentheses (OR (last(init regex_list)) (last regex_list))]) (tail input_string)
                                                                      '|' -> create ')' 1 (init(init opearte)) (init(init regex_list) ++ [OR (last(init regex_list)) (last regex_list)]) (tail input_string)
                                                                      '.' -> create ')' 1 (init (init opearte)) (init(init regex_list) ++ [Parentheses (OR (last(init regex_list)) (last regex_list))]) (tail input_string)
                                                                      _ -> create ')' 1 (init (init opearte)) (init(init regex_list) ++ [Parentheses (OR (last(init regex_list)) (last regex_list))]) (tail input_string)
                                            False -> case (tail input_string == "") of -- True -> r1(r2|r3) 
                                                        True -> case ((length opearte) == 2) of -- True r1(r2|r3) --False r1|(r2|r3) or (r1(r2|r3)) 
                                                                True -> create ')' 1 (init(init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                False -> case(last(init(init opearte))) of
                                                                           '|' -> create ')' 1 (init(init(init opearte))) (init(init(init regex_list)) ++ [OR (last(init(init regex_list))) (OR (last(init regex_list)) (last regex_list))]) (tail input_string)
                                                                           '(' -> create ')' 1 (init(init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                        False -> case ((length opearte) == 2) of -- True -> r1(r2|r3)* False -> (r1|(r2|r3)) or r1|(r2|r3)*
                                                                    True -> case (head(tail input_string)) of
                                                                              '*' -> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                              '+' -> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                              '?' -> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                              '('-> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (OR (last(init regex_list)) (last regex_list))]) (tail input_string)
                                                                              ')'-> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                              '|'-> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (OR (last(init regex_list)) (last regex_list))]) (tail input_string)
                                                                              '.' -> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                              _ -> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                    False -> case (last(init(init opearte)) == '|') of --True -> (r1|(r2|r3))
                                                                            True -> case (head(tail input_string)) of
                                                                                '*' -> create ')' 1 (init(init(init opearte))) (init(init(init regex_list)) ++ [OR (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                                '+' -> create ')' 1 (init(init (init opearte))) (init(init(init regex_list)) ++ [OR (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                                '?' -> create ')' 1 (init(init (init opearte))) (init(init(init regex_list)) ++ [OR (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                                '|' -> create ')' 1 (init(init(init opearte))) (init(init regex_list) ++ [OR (last(init(init regex_list))) (OR (last(init regex_list)) (last regex_list))]) (tail input_string)   
                                                                                '(' -> create ')' 1 (init(init (init opearte))) (init(init(init regex_list)) ++ [OR (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                                ')' -> create ')' 1 (init(init (init opearte))) (init(init(init regex_list)) ++ [OR (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)   
                                                                                '.' -> create ')' 1 (init(init (init opearte))) (init(init(init regex_list)) ++ [OR (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                                _ -> create ')' 1 (init(init (init opearte))) (init(init(init regex_list)) ++ [OR (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                            False -> case (head(tail input_string)) of
                                                                                '*' -> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                                '+' -> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                                '?' -> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                                '(' -> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                                ')' -> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                                '|'-> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (OR (last(init regex_list)) (last regex_list))]) (tail input_string)
                                                                                '.' -> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                                _ -> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                                
                                    '(' -> case (length regex_list == 1) of
                                              True -> case (tail input_string == "") of  --(r)
                                                          True -> create ')' 1 (init opearte) regex_list (tail input_string)
                                                          False -> case (head (tail input_string)) of 
                                                                      '*' -> create ')' 1 (init opearte) (init regex_list ++ [Parentheses (last regex_list)]) (tail input_string)
                                                                      '+' -> create ')' 1 (init opearte) (init regex_list ++ [Parentheses (last regex_list)]) (tail input_string)
                                                                      '?' -> create ')' 1 (init opearte) (init regex_list ++ [Parentheses (last regex_list)]) (tail input_string)
                                                                      _ -> create ')' 1 (init opearte) regex_list (tail input_string)
                                              False -> case (tail input_string == "") of --r|()
                                                          True -> case (init opearte == "|") of 
                                                                    True ->  create ')' 1 (init(init opearte)) (init(init regex_list) ++ [OR (last(init regex_list)) (last regex_list)]) (tail input_string)
                                                                    False -> create ')' 1 (init opearte) (init(init regex_list) ++ [Concat (last(init regex_list)) (last regex_list)]) (tail input_string)
                                                          False -> case (init opearte == "") of --r(r) -> True r|(r)--> False
                                                                      True -> case (head(tail input_string)) of
                                                                              --need to separe to case r(r) r|(r) (a(b)*)
                                                                              '*' -> create ')' 1 (init opearte) (init(init regex_list) ++ [Concat (last(init regex_list)) (Parentheses (last regex_list))]) (tail input_string)
                                                                              '+' -> create ')' 1 (init opearte) (init(init regex_list) ++ [Concat (last(init regex_list)) (Parentheses (last regex_list))]) (tail input_string)
                                                                              '?' -> create ')' 1 (init opearte) (init(init regex_list) ++ [Concat (last(init regex_list)) (Parentheses (last regex_list))]) (tail input_string)
                                                                              _ -> create ')' 1 (init opearte) (init(init regex_list) ++ [Concat (last(init regex_list)) (last regex_list)]) (tail input_string)
                                                                      False -> case (last(init opearte)) of
                                                                                  '|' -> case (head(tail input_string)) of
                                                                                          '*' -> create ')' 1 (init (init opearte)) (init(init regex_list) ++ [OR (last(init regex_list)) (Parentheses (last regex_list))]) (tail input_string)
                                                                                          '+' -> create ')' 1 (init (init opearte)) (init(init regex_list) ++ [OR (last(init regex_list)) (Parentheses (last regex_list))]) (tail input_string)
                                                                                          '?' -> create ')' 1 (init (init opearte)) (init(init regex_list) ++ [OR (last(init regex_list)) (Parentheses (last regex_list))]) (tail input_string)
                                                                                          _ -> create ')' 1 (init (init opearte)) (init(init regex_list) ++ [OR (last(init regex_list)) (last regex_list)]) (tail input_string)
                                                                                  '(' -> case (head(tail input_string)) of
                                                                                          '*' -> create ')' 1 (init opearte) (init(init regex_list) ++ [Concat (last(init regex_list)) (Parentheses (last regex_list))]) (tail input_string)
                                                                                          '+' -> create ')' 1 (init opearte) (init(init regex_list) ++ [Concat (last(init regex_list)) (Parentheses (last regex_list))]) (tail input_string)
                                                                                          '?' -> create ')' 1 (init opearte) (init(init regex_list) ++ [Concat (last(init regex_list)) (Parentheses (last regex_list))]) (tail input_string)
                                                                                          _ -> create ')' 1 (init opearte) (init(init regex_list) ++ [Concat (last(init regex_list)) (last regex_list)]) (tail input_string)
                          '(' -> create '(' 1 (opearte ++ ['(']) regex_list (tail input_string)
                          _ -> create (head input_string) 1 opearte (init(regex_list) ++ [Concat (last regex_list) (Word (head input_string))]) (tail input_string)


                '.' -> case (head input_string) of
                          '.' -> create '.' 1 opearte (init(regex_list) ++ [Concat (last regex_list) (Dot)]) (tail input_string)
                          '*' -> create '*' 1 opearte (init(regex_list) ++ [Star (last regex_list)]) (tail input_string) 
                          '+' -> create '+' 1 opearte (init(regex_list) ++ [Plus (last regex_list)]) (tail input_string) 
                          '?' -> create '?' 1 opearte (init(regex_list) ++ [Question (last regex_list)]) (tail input_string) 
                          '|' -> create '|' 1 (opearte ++ [(head input_string)]) regex_list (tail input_string)  
                          ')' -> case (last opearte) of
                                    '|' -> case (length regex_list == 2) of 
                                            True -> case (tail input_string == "") of --(r1|r2)
                                                        True -> create ')' 1 (init(init opearte)) (init(init regex_list) ++ [OR (last(init regex_list)) (last regex_list)]) (tail input_string)
                                                        False -> case (head (tail input_string)) of --(r1|r2)*
                                                                      '*' -> create ')' 1 (init (init opearte)) (init(init regex_list) ++ [Parentheses (OR (last(init regex_list)) (last regex_list))]) (tail input_string)
                                                                      '+' -> create ')' 1 (init (init opearte)) (init(init regex_list) ++ [Parentheses (OR (last(init regex_list)) (last regex_list))]) (tail input_string)
                                                                      '?' -> create ')' 1 (init (init opearte)) (init(init regex_list) ++ [Parentheses (OR (last(init regex_list)) (last regex_list))]) (tail input_string)
                                                                      ')' -> create ')' 1 (init (init opearte)) (init(init regex_list) ++ [Parentheses (OR (last(init regex_list)) (last regex_list))]) (tail input_string)
                                                                      '(' -> create ')' 1 (init (init opearte)) (init(init regex_list) ++ [Parentheses (OR (last(init regex_list)) (last regex_list))]) (tail input_string)
                                                                      '|' -> create ')' 1 (init(init opearte)) (init(init regex_list) ++ [OR (last(init regex_list)) (last regex_list)]) (tail input_string)
                                                                      '.' -> create ')' 1 (init (init opearte)) (init(init regex_list) ++ [Parentheses (OR (last(init regex_list)) (last regex_list))]) (tail input_string)
                                                                      _ -> create ')' 1 (init (init opearte)) (init(init regex_list) ++ [Parentheses (OR (last(init regex_list)) (last regex_list))]) (tail input_string)
                                            False -> case (tail input_string == "") of -- True -> r1(r2|r3) 
                                                        True -> case ((length opearte) == 2) of -- True r1(r2|r3) --False r1|(r2|r3) or (r1(r2|r3)) 
                                                                True -> create ')' 1 (init(init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                False -> case(last(init(init opearte))) of
                                                                           '|' -> create ')' 1 (init(init(init opearte))) (init(init(init regex_list)) ++ [OR (last(init(init regex_list))) (OR (last(init regex_list)) (last regex_list))]) (tail input_string)
                                                                           '(' -> create ')' 1 (init(init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                        False -> case ((length opearte) == 2) of -- True -> r1(r2|r3)* False -> (r1|(r2|r3)) or r1|(r2|r3)*
                                                                    True -> case (head(tail input_string)) of
                                                                              '*' -> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                              '+' -> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                              '?' -> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                              '('-> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (OR (last(init regex_list)) (last regex_list))]) (tail input_string)
                                                                              ')'-> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                              '|'-> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (OR (last(init regex_list)) (last regex_list))]) (tail input_string)
                                                                              '.' -> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                              _ -> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                    False -> case (last(init(init opearte)) == '|') of --True -> (r1|(r2|r3))
                                                                            True -> case (head(tail input_string)) of
                                                                                '*' -> create ')' 1 (init(init(init opearte))) (init(init(init regex_list)) ++ [OR (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                                '+' -> create ')' 1 (init(init (init opearte))) (init(init(init regex_list)) ++ [OR (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                                '?' -> create ')' 1 (init(init (init opearte))) (init(init(init regex_list)) ++ [OR (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                                '|' -> create ')' 1 (init(init(init opearte))) (init(init regex_list) ++ [OR (last(init(init regex_list))) (OR (last(init regex_list)) (last regex_list))]) (tail input_string)   
                                                                                '(' -> create ')' 1 (init(init (init opearte))) (init(init(init regex_list)) ++ [OR (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                                ')' -> create ')' 1 (init(init (init opearte))) (init(init(init regex_list)) ++ [OR (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)   
                                                                                '.' -> create ')' 1 (init(init (init opearte))) (init(init(init regex_list)) ++ [OR (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                                _ -> create ')' 1 (init(init (init opearte))) (init(init(init regex_list)) ++ [OR (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                            False -> case (head(tail input_string)) of
                                                                                '*' -> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                                '+' -> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                                '?' -> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                                '(' -> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                                ')' -> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                                '|'-> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (OR (last(init regex_list)) (last regex_list))]) (tail input_string)
                                                                                '.' -> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                                _ -> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                                
                                    '(' -> case (length regex_list == 1) of
                                              True -> case (tail input_string == "") of  --(r)
                                                          True -> create ')' 1 (init opearte) regex_list (tail input_string)
                                                          False -> case (head (tail input_string)) of 
                                                                      '*' -> create ')' 1 (init opearte) (init regex_list ++ [Parentheses (last regex_list)]) (tail input_string)
                                                                      '+' -> create ')' 1 (init opearte) (init regex_list ++ [Parentheses (last regex_list)]) (tail input_string)
                                                                      '?' -> create ')' 1 (init opearte) (init regex_list ++ [Parentheses (last regex_list)]) (tail input_string)
                                                                      _ -> create ')' 1 (init opearte) regex_list (tail input_string)
                                              False -> case (tail input_string == "") of --r|()
                                                          True -> case (init opearte == "|") of 
                                                                    True ->  create ')' 1 (init(init opearte)) (init(init regex_list) ++ [OR (last(init regex_list)) (last regex_list)]) (tail input_string)
                                                                    False -> create ')' 1 (init opearte) (init(init regex_list) ++ [Concat (last(init regex_list)) (last regex_list)]) (tail input_string)
                                                          False -> case (init opearte == "") of --r(r) -> True r|(r)--> False
                                                                      True -> case (head(tail input_string)) of
                                                                              --need to separe to case r(r) r|(r) (a(b)*)
                                                                              '*' -> create ')' 1 (init opearte) (init(init regex_list) ++ [Concat (last(init regex_list)) (Parentheses (last regex_list))]) (tail input_string)
                                                                              '+' -> create ')' 1 (init opearte) (init(init regex_list) ++ [Concat (last(init regex_list)) (Parentheses (last regex_list))]) (tail input_string)
                                                                              '?' -> create ')' 1 (init opearte) (init(init regex_list) ++ [Concat (last(init regex_list)) (Parentheses (last regex_list))]) (tail input_string)
                                                                              _ -> create ')' 1 (init opearte) (init(init regex_list) ++ [Concat (last(init regex_list)) (last regex_list)]) (tail input_string)
                                                                      False -> case (last(init opearte)) of
                                                                                  '|' -> case (head(tail input_string)) of
                                                                                          '*' -> create ')' 1 (init (init opearte)) (init(init regex_list) ++ [OR (last(init regex_list)) (Parentheses (last regex_list))]) (tail input_string)
                                                                                          '+' -> create ')' 1 (init (init opearte)) (init(init regex_list) ++ [OR (last(init regex_list)) (Parentheses (last regex_list))]) (tail input_string)
                                                                                          '?' -> create ')' 1 (init (init opearte)) (init(init regex_list) ++ [OR (last(init regex_list)) (Parentheses (last regex_list))]) (tail input_string)
                                                                                          _ -> create ')' 1 (init (init opearte)) (init(init regex_list) ++ [OR (last(init regex_list)) (last regex_list)]) (tail input_string)
                                                                                  '(' -> case (head(tail input_string)) of
                                                                                          '*' -> create ')' 1 (init opearte) (init(init regex_list) ++ [Concat (last(init regex_list)) (Parentheses (last regex_list))]) (tail input_string)
                                                                                          '+' -> create ')' 1 (init opearte) (init(init regex_list) ++ [Concat (last(init regex_list)) (Parentheses (last regex_list))]) (tail input_string)
                                                                                          '?' -> create ')' 1 (init opearte) (init(init regex_list) ++ [Concat (last(init regex_list)) (Parentheses (last regex_list))]) (tail input_string)
                                                                                          _ -> create ')' 1 (init opearte) (init(init regex_list) ++ [Concat (last(init regex_list)) (last regex_list)]) (tail input_string)
                          '(' -> create '(' 1 (opearte ++ ['(']) regex_list (tail input_string)
                          _ -> create (head input_string) 1 opearte (init(regex_list) ++ [Concat (last regex_list) (Word (head input_string))]) (tail input_string)
                _  -> case (head input_string) of
                          '.' -> create '.' 1 opearte (init(regex_list) ++ [Concat (last regex_list) (Dot)]) (tail input_string)
                          '*' -> create '*' 1 opearte (init(regex_list) ++ [Star (last regex_list)]) (tail input_string) 
                          '+' -> create '+' 1 opearte (init(regex_list) ++ [Plus (last regex_list)]) (tail input_string) 
                          '?' -> create '?' 1 opearte (init(regex_list) ++ [Question (last regex_list)]) (tail input_string) 
                          '|' -> create '|' 1 (opearte ++ [(head input_string)]) regex_list (tail input_string) 
                          ')' -> case (last opearte) of
                                    '|' -> case (length regex_list == 2) of 
                                            True -> case (tail input_string == "") of --(r1|r2)
                                                        True -> create ')' 1 (init(init opearte)) (init(init regex_list) ++ [OR (last(init regex_list)) (last regex_list)]) (tail input_string)
                                                        False -> case (head (tail input_string)) of --(r1|r2)*
                                                                      '*' -> create ')' 1 (init (init opearte)) (init(init regex_list) ++ [Parentheses (OR (last(init regex_list)) (last regex_list))]) (tail input_string)
                                                                      '+' -> create ')' 1 (init (init opearte)) (init(init regex_list) ++ [Parentheses (OR (last(init regex_list)) (last regex_list))]) (tail input_string)
                                                                      '?' -> create ')' 1 (init (init opearte)) (init(init regex_list) ++ [Parentheses (OR (last(init regex_list)) (last regex_list))]) (tail input_string)
                                                                      ')' -> create ')' 1 (init (init opearte)) (init(init regex_list) ++ [Parentheses (OR (last(init regex_list)) (last regex_list))]) (tail input_string)
                                                                      '(' -> create ')' 1 (init (init opearte)) (init(init regex_list) ++ [Parentheses (OR (last(init regex_list)) (last regex_list))]) (tail input_string)
                                                                      '|' -> create ')' 1 (init(init opearte)) (init(init regex_list) ++ [OR (last(init regex_list)) (last regex_list)]) (tail input_string)
                                                                      '.' -> create ')' 1 (init (init opearte)) (init(init regex_list) ++ [Parentheses (OR (last(init regex_list)) (last regex_list))]) (tail input_string)
                                                                      _ -> create ')' 1 (init (init opearte)) (init(init regex_list) ++ [Parentheses (OR (last(init regex_list)) (last regex_list))]) (tail input_string)
                                            False -> case (tail input_string == "") of -- True -> r1(r2|r3) 
                                                        True -> case ((length opearte) == 2) of -- True r1(r2|r3) --False r1|(r2|r3) or (r1(r2|r3)) 
                                                                True -> create ')' 1 (init(init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                False -> case(last(init(init opearte))) of
                                                                           '|' -> create ')' 1 (init(init(init opearte))) (init(init(init regex_list)) ++ [OR (last(init(init regex_list))) (OR (last(init regex_list)) (last regex_list))]) (tail input_string)
                                                                           '(' -> create ')' 1 (init(init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                        False -> case ((length opearte) == 2) of -- True -> r1(r2|r3)* False -> (r1|(r2|r3)) or r1|(r2|r3)*
                                                                    True -> case (head(tail input_string)) of
                                                                              '*' -> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                              '+' -> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                              '?' -> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                              '('-> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (OR (last(init regex_list)) (last regex_list))]) (tail input_string)
                                                                              ')'-> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                              '|'-> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (OR (last(init regex_list)) (last regex_list))]) (tail input_string)
                                                                              '.' -> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                              _ -> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                    False -> case (last(init(init opearte)) == '|') of --True -> (r1|(r2|r3))
                                                                            True -> case (head(tail input_string)) of
                                                                                '*' -> create ')' 1 (init(init(init opearte))) (init(init(init regex_list)) ++ [OR (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                                '+' -> create ')' 1 (init(init (init opearte))) (init(init(init regex_list)) ++ [OR (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                                '?' -> create ')' 1 (init(init (init opearte))) (init(init(init regex_list)) ++ [OR (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                                '|' -> create ')' 1 (init(init(init opearte))) (init(init regex_list) ++ [OR (last(init(init regex_list))) (OR (last(init regex_list)) (last regex_list))]) (tail input_string)   
                                                                                '(' -> create ')' 1 (init(init (init opearte))) (init(init(init regex_list)) ++ [OR (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                                ')' -> create ')' 1 (init(init (init opearte))) (init(init(init regex_list)) ++ [OR (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)   
                                                                                '.' -> create ')' 1 (init(init (init opearte))) (init(init(init regex_list)) ++ [OR (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                                _ -> create ')' 1 (init(init (init opearte))) (init(init(init regex_list)) ++ [OR (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                            False -> case (head(tail input_string)) of
                                                                                '*' -> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                                '+' -> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                                '?' -> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                                '(' -> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                                ')' -> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                                '|'-> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (OR (last(init regex_list)) (last regex_list))]) (tail input_string)
                                                                                '.' -> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                                _ -> create ')' 1 (init (init opearte)) (init(init(init regex_list)) ++ [Concat (last(init(init regex_list))) (Parentheses (OR (last(init regex_list)) (last regex_list)))]) (tail input_string)
                                                                                
                                    '(' -> case (length regex_list == 1) of
                                              True -> case (tail input_string == "") of  --(r)
                                                          True -> create ')' 1 (init opearte) regex_list (tail input_string)
                                                          False -> case (head (tail input_string)) of 
                                                                      '*' -> create ')' 1 (init opearte) (init regex_list ++ [Parentheses (last regex_list)]) (tail input_string)
                                                                      '+' -> create ')' 1 (init opearte) (init regex_list ++ [Parentheses (last regex_list)]) (tail input_string)
                                                                      '?' -> create ')' 1 (init opearte) (init regex_list ++ [Parentheses (last regex_list)]) (tail input_string)
                                                                      _ -> create ')' 1 (init opearte) regex_list (tail input_string)
                                              False -> case (tail input_string == "") of --r|()
                                                          True -> case (init opearte == "|") of 
                                                                    True ->  create ')' 1 (init(init opearte)) (init(init regex_list) ++ [OR (last(init regex_list)) (last regex_list)]) (tail input_string)
                                                                    False -> create ')' 1 (init opearte) (init(init regex_list) ++ [Concat (last(init regex_list)) (last regex_list)]) (tail input_string)
                                                          False -> case (init opearte == "") of --r(r) -> True r|(r)--> False
                                                                      True -> case (head(tail input_string)) of
                                                                              --need to separe to case r(r) r|(r) (a(b)*)
                                                                              '*' -> create ')' 1 (init opearte) (init(init regex_list) ++ [Concat (last(init regex_list)) (Parentheses (last regex_list))]) (tail input_string)
                                                                              '+' -> create ')' 1 (init opearte) (init(init regex_list) ++ [Concat (last(init regex_list)) (Parentheses (last regex_list))]) (tail input_string)
                                                                              '?' -> create ')' 1 (init opearte) (init(init regex_list) ++ [Concat (last(init regex_list)) (Parentheses (last regex_list))]) (tail input_string)
                                                                              _ -> create ')' 1 (init opearte) (init(init regex_list) ++ [Concat (last(init regex_list)) (last regex_list)]) (tail input_string)
                                                                      False -> case (last(init opearte)) of
                                                                                  '|' -> case (head(tail input_string)) of
                                                                                          '*' -> create ')' 1 (init (init opearte)) (init(init regex_list) ++ [OR (last(init regex_list)) (Parentheses (last regex_list))]) (tail input_string)
                                                                                          '+' -> create ')' 1 (init (init opearte)) (init(init regex_list) ++ [OR (last(init regex_list)) (Parentheses (last regex_list))]) (tail input_string)
                                                                                          '?' -> create ')' 1 (init (init opearte)) (init(init regex_list) ++ [OR (last(init regex_list)) (Parentheses (last regex_list))]) (tail input_string)
                                                                                          _ -> create ')' 1 (init (init opearte)) (init(init regex_list) ++ [OR (last(init regex_list)) (last regex_list)]) (tail input_string)
                                                                                  '(' -> case (head(tail input_string)) of
                                                                                          '*' -> create ')' 1 (init opearte) (init(init regex_list) ++ [Concat (last(init regex_list)) (Parentheses (last regex_list))]) (tail input_string)
                                                                                          '+' -> create ')' 1 (init opearte) (init(init regex_list) ++ [Concat (last(init regex_list)) (Parentheses (last regex_list))]) (tail input_string)
                                                                                          '?' -> create ')' 1 (init opearte) (init(init regex_list) ++ [Concat (last(init regex_list)) (Parentheses (last regex_list))]) (tail input_string)
                                                                                          _ -> create ')' 1 (init opearte) (init(init regex_list) ++ [Concat (last(init regex_list)) (last regex_list)]) (tail input_string)
                          '(' -> create '(' 1 (opearte ++ ['(']) regex_list (tail input_string)
                          _ -> create (head input_string) 1 opearte (init(regex_list) ++ [Concat (last regex_list) (Word (head input_string))]) (tail input_string)
  | return = [((last regex_list), input_string)] -- end of the loop
  where 
    return0 = (start == 0)
    return1 = (start == 1) && (input_string /= "")
    return = input_string == ""
{-
   Reminder for the create funciton ')' have set many nested case of for different suitation to consider to
   add a Regex Partheneses for printing out
-}

{-
   Stopping all wrong regex input
-}
check :: Char -> Int -> Int -> String -> Bool
check previous start num_open input
  | return0 = False -- online one single character and is + * ? | ) 
  | end2 = False -- start with * + ? | )
  | return1 = check previous 1 1 (input) -- start with (
  | return2 = check previous 1 0 (input) -- start with other character
  | end = False -- end with ( |
  | end1 = True
  | end3 = False
  | return3 = False -- previous char * + ? and current char * + ? 
  | return4 = False -- previous char ( and current char * + ? |
  | return5 = False -- previous char | and current char * + ? ) | 
  | return6 = False -- not enough ( at the previous stirng for )
  | return7 = check (head input) 1 (num_open - 1) (tail input) -- enought ( for current )
  | return8 = check (head input) 1 (num_open + 1) (tail input)
  | otherwise = check (head input) 1 (num_open) (tail input)
  where
    return0 = (start == 0) && (input == "") && (previous == '*' || previous == '+' || previous == ')' || previous == '|' || previous == '?')
    end2 = (start == 0) && (previous == '*' || previous == '+' || previous == ')' || previous == '|' || previous == '?')
    return1 = (start == 0) && (input /= "") && (previous == '(')
    return2 = (start == 0) && (input /= "") && (previous /= '(')
    end = (input == "") && (previous == '(' || previous == '|')
    end1 = (input == "") && (num_open == 0)
    end3 = (input == "") && (num_open /= 0)
    return3 = (previous == '*' || previous == '+' || previous == '?') && ((head input) == '*' || (head input) == '+' || (head input) == '?')
    return4 = (previous == '(') &&  ((head input) == '*' || (head input) == '+' || (head input) == '?' || (head input) == '|')
    return5 = (previous == '|') &&  ((head input) == '*' || (head input) == '+' || (head input) == '?' || (head input) == '|' || (head input) == ')')
    return6 = ((head input) == ')') && (num_open == 0)
    return7 = ((head input) == ')') && (num_open > 0)
    return8 = ((head input) == '(') 




  


{- * Question 8. (25 pts)

Replace `undefined` with your implementation

-}

matcher :: Regex -> Parser String
matcher regex = parser(\s -> (maker (makeList s) [] regex))

{-
  Recusion through the list of tuples to see whether the tuples can stay by using match fucntion 
  True  will keep the tuples
  False will remove the tuples
-}
maker :: [(String, String)] -> [(String, String)] -> Regex -> [(String, String)]
maker input output regex
   | return    = output
   | append    = maker (tail input) (output ++ [(head input)]) regex
   | otherwise = maker (tail input) output regex
   where
     return = input == []
     append = matching regex (fst(head input)) 
{-
   makeList series is to List out all the possible cases of a string 
   example "ab" --> [("","ab"),("a","b"),("ab",""),("","b"),("b",""),("","")]
-}
makeList :: String -> [(String, String)]
makeList input = makeList3 (makeList1 input []) []

makeList1 :: String -> [String] -> [String]
makeList1 input output
  | return = output
  | otherwise = makeList1 (tail input) (output ++ [input])
  where
    return = input == ""

makeList2 :: String -> String -> [(String, String)] -> [(String, String)]
makeList2 match input output
  | return    = output ++ [(match, "")]
  | ready     = makeList2 (match ++ [(head input)]) (tail input) (output ++ [("", input)])
  | otherwise = makeList2 (match ++ [(head input)]) (tail input) (output ++ [(match, input)])
  where 
    return = input == ""
    ready  = match == ""

makeList3 :: [String] -> [(String, String)] -> [(String, String)]
makeList3 input output
  | return = output ++ [("","")]
  | otherwise = makeList3 (tail input) (output ++ (makeList2 "" (head input) []))
  where
    return = input == []



-- use show to covert back to string
-- maybe this funciton will get some trouble, because of the regexP 
-- please comment out the paremetenses in the valid fucniton
-- hand type the regex yourself like a(b|c) --> Concat (Word 'a') (OR (Word 'b') (Word 'c'))

--need to be hand type have bug
-- regexOf :: String -> Regex
-- regexOf input = fst $ head (runParser regexP input)


--check regex is match string or not
{-
   Added an Empty Regex for the case the regex has ran up
-}
{-
 Take a function as parament so that can take the remaining string as input to loop in the case of concat
 http://learnyouahaskell.com/higher-order-functions#:~:text=Haskell%20functions%20can%20take%20functions,much%20are%20the%20Haskell%20experience.
 Using Lambda expression to create an anonymous function to pass the remaining string until the null is passed.
 https://wiki.haskell.org/Anonymous_function
 For the concat case 
 split to see how many string the former part of concat will match --> use lambda expression to pass a funciton 
 therefore no need split in to such many cases to check 
 https://people.inf.ethz.ch/mraszyk/fpcomp19/sol_5.html
 https://coursys.sfu.ca/2018sp-cmpt-384-d1/pages/HaskellRE1
-}

--comment out "valid (Parentheses e)   xs   k = valid e xs k" this if got some troubles 
valid :: Regex -> String -> (String -> Bool) -> Bool  -- worker function
valid Empty          xs   k = False --regex end up
valid (Parentheses e)   xs   k = valid e xs k
valid (Word c)   (x:xs)   k = c==x && k xs --
valid Dot        (x:xs)   k = k xs
valid (Word c)       []   k = False -- the input string become empty
valid Dot            []   k = False -- the input string become empty
valid (Concat e1 e2) xs   k = valid e1 xs (\xs' -> valid e2 xs' k)
valid (OR e1 e2)     xs   k = valid e1 xs k || valid e2 xs k
valid (Question e)   xs   k = k xs || valid e xs k-- 0 or 1
valid (Plus e)       xs   k = valid e xs (\xs' -> valid (Star e) xs' k)  -- valid e xs which is do once then pass to do many or zero
valid (Star e)       xs   k = validMany e xs k
  where 
     validMany e xs k = k xs || valid e xs (\xs' -> xs'/=xs && validMany e xs' k)  
  
{-
   Using Regex data type for matching will not need to consider the string regex input
-}
matching :: Regex -> String -> Bool
matching regex target = valid regex target null
 

closingIndex :: String -> Int -> Int -> Int
closingIndex input num_open index 
   | return    = index
   | isopen    = closingIndex (tail input) (num_open + 1) (index + 1)
   | isclose   = closingIndex (tail input) (num_open - 1) (index + 1)
   | otherwise = closingIndex (tail input) (num_open) (index + 1)
   where
     return = (num_open == 1) && (head input == ')')
     isopen = (head input == '(')
     isclose = (head input == ')')


matchCanEmpty :: String -> Bool
matchCanEmpty match
   | return0 = False
   | return1 = True
   | return2 = True
   | otherwise = False
   where
     return0 = length match == 1
     return1 = match == ""
     return2 = (length match == 2 && last match == '*') || (length match == 2 && last match == '?') || (((closingIndex match 0 0) == (length match - 2)) && (last match == '*')) || (((closingIndex match 0 0) == (length match - 2)) && (last match == '?'))


