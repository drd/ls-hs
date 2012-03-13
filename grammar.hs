module LGrammar where

import qualified Data.Set as Set
import Data.Set (Set)

{-

Deterministic, Context-Free Production:
p: Letter -> Word

1L Production:
p: (Letter, Context, Direction) -> Word

2L Production:
p: (Maybe Left, Letter, Maybe Right) -> Word

Stochastic Production:
p: (DeterministicProduction, Probability) -> Word


class Production where


class Predecessor p where
  --> :: p -> String -> Production

instance Predecessor String where
  p --> s = 

(<:) :: String -> Predecessor p

axiom "F"
"F"               --> "F+F+F+F+"
"F" <: "G" >: "H" --> "GG"





-}

type Word        = String

data Context     = LeftOf  Word
                 | RightOf Word

data Predecessor = OL  Char

                 | IL  { pred        :: Char,
                         context     :: Context
                       }

                 | IIL { left, right :: Word,
                         pred        :: Char
                       }


-- data Production = ( Predecessor, Word )



data Production = Pr { pre, suc  :: String }
                  deriving Show

data LGrammar   = Lg { axiom       :: String,
                       productions :: [Production] }
                  deriving Show

{-

generate the nth iteration of the LGrammar

for each production, start with an empty string accumulator
attempt to match each production against the beginning of the
axiom / previous production

if none of the productions match, then cons the first character
of the axiom to the accumulator, and then repeat this process
with the tail of the axiom

if one of the productions match, cons the successor of the production
to the accumulator, and repeat the process with the length(pred p)
character of the axiom

-}

generate                :: LGrammar -> Int -> String
generate  lg n           = produce (axiom lg) n
    where produce s 0    = s
          produce s n    = produce (next s) (n-1)

          next []        = ""
          next (x:xs)    = (suc' x) ++ (next xs)
          -- next only works on 1-character variables
          -- to generalize productions, suc' will have
          -- to return a pair of strings (predecessor, tail)
          -- and then next = (fst suc') ++ (next (snd suc'))

          suc' x = suc (applicableProduction [x] (productions lg))

          applicableProduction s []     = Pr s s
          applicableProduction s (p:ps)
              | (pre p) == s = p
              | otherwise    = applicableProduction s ps


-- TODO:
-- - make x,y a point
-- use pattern matching on children to destructure state into components
-- make a helper function to wrap actions & eliminate boilerplate
-- make theta/heading radians
-- actually move in the direction of heading


data TurtleState = S {
      x, y, theta, heading :: Float
    } deriving Show


initialState       :: Float -> [TurtleState]
initialState theta  = [ S 0 0 theta 90 ]

up   :: TurtleState -> IO TurtleState
up s  = return s

forward        :: [TurtleState] -> [TurtleState]
forward (s:ss)  = (S (x s + 1) (y s) (theta s) (heading s)) : ss

rotatePos         :: [TurtleState] -> [TurtleState]
rotatePos (s:ss)   = (S (x s) (y s) (theta s) (theta s + heading s)) : ss

rotateNeg         :: [TurtleState] -> [TurtleState]
rotateNeg (s:ss)   = (S (x s) (y s) (theta s) (heading s - theta s)) : ss

push        :: [TurtleState] -> [TurtleState]
push (s:ss)  = (S (x s) (y s) (theta s) (heading s)) : s : ss

pop        :: [TurtleState] -> [TurtleState]
pop (s:ss)  = ss

actOn     :: Char -> IO [TurtleState] -> IO [TurtleState]
actOn c s  = do states <- s
                let s' = case c of
                           'F' -> forward states
                           '+' -> rotatePos states
                           '-' -> rotateNeg states
                           '[' -> push states
                           ']' -> pop states
                return s'


interpret         :: String -> Float -> IO [TurtleState]
interpret s theta  = interpret' s (return $ initialState theta)
    where interpret' [] state     = state
          interpret' (x:xs) state = return (actOn x state)
                                    >>= interpret' xs

{-
foldr (\c s -> >>=) state (\c -> actOn c state)
this was an attempt to get something working, without
using the full datastructure:

-}

produce'         :: String -> Int -> String
produce' s 0      = s
produce' s n      = produce' (next s) (n-1)
    where next []     = ""
          next (x:xs) = case x of
                          'F' -> "F+F-F+F" ++ next xs
                          _   -> x:(next xs)

