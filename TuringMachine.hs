{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module TuringMachine where

import Data.Maybe

--The a's are the symbols of the tape, b is the state
data Tape a b = Tape [Symbol a] (Symbol a) b [Symbol a] 
      
data Direction = L | R | N 
   deriving (Show, Eq)
   
type Symbol a = Maybe a

type TapeAction a b = Maybe (Symbol a, b, Direction)

type Transition a b = (Symbol a, b) -> TapeAction a b

operate :: Transition a b -> Maybe (Tape a b) -> Maybe (Tape a b)
operate f tape = doTapeAction (getTapeAction f tape) tape

getTapeAction' :: Transition a b -> Tape a b -> TapeAction a b
getTapeAction' f (Tape _ symb state _) = f (symb, state)

getTapeAction :: Transition a b -> Maybe (Tape a b) -> TapeAction a b
getTapeAction f (Just (Tape _ symb state _)) = f (symb, state)
getTapeAction _ Nothing                      = Nothing

doTapeAction :: TapeAction a b -> Maybe (Tape a b) -> Maybe (Tape a b)
doTapeAction (Just (symb, state, dir)) (Just (Tape (y : ys) _ _ next))
   |dir == L =  Just $ Tape ys y state (symb : next)
doTapeAction (Just (symb, state, dir)) (Just (Tape prev _ _ (x : xs))) 
   |dir == R =  Just $ Tape (symb:prev) x state xs
doTapeAction (Just (symb, state, dir)) (Just (Tape prev _ _ next))
   |dir == R =  Just $ Tape (symb : prev) Nothing state []
   |dir == N =  Just $ Tape prev symb state next
   |dir == L =  Just $ Tape [] Nothing state (symb : next)
doTapeAction _ _ = Nothing

run :: Transition a b -> Maybe (Tape a b) -> [Tape a b]
run f = catMaybes . takeWhile isJust . iterate (operate f)

moves :: Transition a b -> [Tape a b] -> [Direction]
moves f = (map third) . catMaybes . map (getTapeAction' f)

third :: (a, b, c) -> c
third (_, _, x) = x

runAndMoves :: Transition a b -> Maybe (Tape a b) -> [(Direction, Tape a b, Direction)]
runAndMoves f tape = zip3 (N : (moves f $ run f tape)) (run f tape) ((moves f $ run f tape) ++ [N])