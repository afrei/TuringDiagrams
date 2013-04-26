{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module TuringAnimations (animTape, animTM) where

import Data.Active
import TuringDiagrams
import TuringMachine
import Diagrams.Prelude

stationary :: Active (V (D2 be))
upTo, upTo' :: Double -> Active (V (D2 be))
stationary = (const 0) <$> ui
upTo' d  = (\x -> r2 (d * x, 0)) <$> (\x -> -x) <$> ui
upTo  d  = (\x -> r2 (d * x, 0)) <$>                ui

animTape :: (D2 be -> D2 be -> D2 be) -> (Symbol a -> D2 be) -> (b -> D2 be) -> (Tape a b, Direction) -> Active (D2 be)
animTape f g h (t, N) = transID f g h <$> stationary                 <*> pure t
animTape f g h (t, R) = transID f g h <$> upTo' (width $ g $ symb t) <*> pure t
animTape f g h (t, L) = transID f g h <$> upTo  (width $ g $ symb t) <*> pure t

animTM :: (D2 be -> D2 be -> D2 be) -> (Symbol a -> D2 be) -> (b -> D2 be) -> Transition a b -> Tape a b -> Active (D2 be)
animTM cmb symF sttF trns tp = movie $ map (animTape cmb symF sttF) . (runAndMoves trns) $ Just tp