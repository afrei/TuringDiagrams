{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module TuringDiagrams where

import TuringMachine
import Diagrams.Prelude hiding (cat') --I hid cat' because I really wanted the analogy (cat : cat' :: beside : beside') see below
import Diagrams.Backend.SVG

type D2 = Diagram SVG R2 

{- tapeToDia takes 4 arguments, in the following order:
1. a function D2 -> D2 -> D2 which will take as its first argument a D2 representing the tape as it is and second a D2 
representing the current state and returning a D2 representing the combination (we know the center of the tape is where the pointer is)
2. a function Symbol a -> D2 which will produce pictures corresponding to symbols
3. a function b -> D2 which will produce pictures corresponding to states
4. a Tape a b which will be turned into a diagram using the above arguments -}

tapeToDia :: (D2 -> D2 -> D2) -> (Symbol a -> D2) -> (b -> D2) -> Tape a b -> D2
tapeToDia cmCurSt symf statef tape @ (Tape _ _ state _) = 
    cmCurSt (statef state) $ makeTape symf tape
	
transTapeToDia :: (D2 -> D2 -> D2) -> (Symbol a -> D2) -> (b -> D2) -> V D2 -> Tape a b -> D2
transTapeToDia cmCurSt symf statef v tape @ (Tape _ _ state _) = 
    cmCurSt (statef state) $ translate v $ makeTape symf tape
    
makeTape :: (Symbol a -> D2) -> Tape a b -> D2
makeTape symf (Tape prv symb _ nxt) = 
    cat' right $ (map symf . reverse $ prv) ++ [cat right $ symf symb : map symf nxt]
    
beside' :: V D2 -> D2 -> D2 -> D2
beside' = flip . beside . negateV

cat' :: R2 -> [D2] -> D2
cat' v2 = foldr (beside' v2) mempty

tapesToDia :: (D2 -> D2 -> D2) -> (Symbol a -> D2) -> (b -> D2) -> [Tape a b] -> D2
tapesToDia comb symbF statF = cat down . map (tapeToDia comb symbF statF)

down, right :: V D2
down = (r2 (0, -1))
right = (r2 (1, 0))