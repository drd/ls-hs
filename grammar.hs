module LGrammar where

import qualified Data.Set as Set
import Data.Set (Set)

data Production = Pr { pre, suc  :: String }
                  deriving Show

data LGrammar   = Lg { alphabet    :: Set(Char),
                       axiom       :: String,
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

{-

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

