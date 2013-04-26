{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module TuringDiagrams (D2, drawID, transID, drawTapeList) where

import TuringMachine
import Diagrams.Prelude hiding (cat') 

type D2 be = Diagram be R2 

{- drawID takes 4 arguments, in the following order:
1. a function D2 be -> D2 be -> D2 be which will take as its first argument a D2 be representing the tape as it is and second a D2 be 
representing the current state and returning a D2 be representing the combination (we know the center of the tape is where the pointer is)
2. a function Symbol a -> D2 be which will produce pictures corresponding to symbols
3. a function b -> D2 be which will produce pictures corresponding to states
4. a Tape a b which will be turned into a diagram using the above arguments -}

drawID :: (D2 be -> D2 be -> D2 be) -> (Symbol a -> D2 be) -> (b -> D2 be) -> Tape a b -> D2 be
drawID comb sym stt tape = drawHead comb stt tape $ drawTape sym tape
	
transID :: (D2 be -> D2 be -> D2 be) -> (Symbol a -> D2 be) -> (b -> D2 be) -> V (D2 be) -> Tape a b -> D2 be
transID comb sym stt v tape = drawHead comb stt tape $ translate v $ drawTape sym tape

drawHead :: (D2 be -> D2 be -> D2 be) -> (b -> D2 be) -> Tape a b -> D2 be -> D2 be
drawHead comb symf tape = comb (symf $ state tape)
    
drawTape :: (Symbol a -> D2 be) -> Tape a b -> D2 be
drawTape symf tape = addLeft symf tape $ drawRight symf tape

addLeft :: (Symbol a -> D2 be) -> Tape a b -> D2 be -> D2 be
addLeft symf tape d2= cat' right $ (map symf . reverse $ prev tape) ++ [d2]

drawRight :: (Symbol a -> D2 be) -> Tape a b -> D2 be
drawRight symf (Tape _ sym _ nxt) = cat right $ symf sym : map symf nxt
    
drawTapeList :: (D2 be -> D2 be -> D2 be) -> (Symbol a -> D2 be) -> (b -> D2 be) -> [Tape a b] -> D2 be
drawTapeList comb symbF statF = cat down . map (drawID comb symbF statF)

down, right :: V (D2 be)
down = (r2 (0, -1))
right = (r2 (1, 0))

beside' :: V (D2 be) -> D2 be -> D2 be -> D2 be
beside' = flip . beside . negateV

cat' :: R2 -> [D2 be] -> D2 be
cat' v2 = foldr (beside' v2) mempty