{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module TuringMachine where

import Data.Maybe
import Control.Monad

--The a's are the symbols of the tape, b is the state
data Tape a b = Tape {prev :: [Symbol a], symb :: Symbol a, state :: b, next :: [Symbol a]}
      
data Direction = L | R | N 
   deriving (Show, Eq)
   
type Symbol a = Maybe a

type TapeAct a b = (Symbol a, b, Direction)

type Transition a b = (Symbol a, b) -> Maybe (TapeAct a b)

operate :: Transition a b -> Maybe (Tape a b) -> Maybe (Tape a b)
operate f tape = (tape >>= getTapeAct f >>= (\x -> return $ doTapeAct x)) `ap` tape

getTapeAct :: Transition a b -> Tape a b -> Maybe (TapeAct a b)
getTapeAct f tape = f (symb tape, state tape)

doTapeAct :: TapeAct a b -> Tape a b -> Tape a b
doTapeAct (sym, stt, L) (Tape (y:ys) _ _ nxt) = Tape ys y stt (sym:nxt)
doTapeAct (sym, stt, R) (Tape prv _ _ (x:xs)) = Tape (sym:prv) x stt xs
doTapeAct (sym, stt, L) (Tape _ _ _ nxt)      = Tape [] Nothing stt (sym:nxt)
doTapeAct (sym, stt, N) (Tape prv _ _ nxt)    = Tape prv sym stt nxt
doTapeAct (sym, stt, R) (Tape prv _ _ _)      = Tape (sym:prv) Nothing stt []

runTM :: Transition a b -> Maybe (Tape a b) -> [Tape a b]
runTM f = catMaybes . takeWhile isJust . iterate (operate f)

runAndMoves :: Transition a b -> Maybe (Tape a b) -> [(Tape a b, Direction)]
runAndMoves f tape = zip (runTM f tape) $ (moves f $ runTM f tape) ++ [N]

moves :: Transition a b -> [Tape a b] -> [Direction]
moves f = (map (\(_, _, x) -> x)) . catMaybes . map (getTapeAct f)