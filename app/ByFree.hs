{-# LANGUAGE DeriveFunctor #-}
module ByFree (Bowling, runPrintFrames, runCalcScore, frame, finalFrame)where

import Control.Monad.Free  

data Frame next = 
    Normal String String next
    | Final String String String
    deriving (Functor) 

type Bowling = Free Frame ()

markedRawPoint :: Int
markedRawPoint = 10

frame :: String -> String -> Bowling
frame a b = liftF $ Normal a b ()

finalFrame :: String -> String -> String -> Bowling
finalFrame a b c = liftF $ Final a b c

runPrintFrames :: Bowling -> IO ()
runPrintFrames (Pure _) = putStrLn "Game End"
runPrintFrames (Free (Normal a b next)) = do
    putStrLn $ "(" ++ a ++ "," ++ b ++ ")"
    runPrintFrames next
runPrintFrames (Free (Final a b c)) = do 
    putStrLn $ "(" ++ a ++ "," ++ b ++ "," ++ c ++ ")"

readRoll :: String -> Int
readRoll "X" = markedRawPoint
readRoll "-" = 0
readRoll "/" = 0
readRoll "G" = 0
readRoll " " = 0
readRoll a = read a :: Int

readRawPoint :: String -> String -> Int
readRawPoint _ "X" = markedRawPoint
readRawPoint _ "/" = markedRawPoint
readRawPoint a b = readRoll a + readRoll b

readRawPointAt10Th :: String -> String -> String -> Int
readRawPointAt10Th "X" "X" r = markedRawPoint * 2 + readRoll r
readRawPointAt10Th "X" a b = markedRawPoint + readRawPoint a b
readRawPointAt10Th _ "/" b = markedRawPoint + readRoll b
readRawPointAt10Th a b _ = readRawPoint a b

bonus :: Bowling -> Int
-- Strike Bonus
bonus (Free (Normal " " "X" (Free (Normal " " "X" (Free (Normal " " "X" _)))))) = markedRawPoint * 2
bonus (Free (Normal " " "X" (Free (Normal " " "X" (Free (Normal a _ _)))))) = markedRawPoint + readRoll a
bonus (Free (Normal " " "X" (Free (Normal " " "X" (Free (Final a _ _)))))) = markedRawPoint + readRoll a
bonus (Free (Normal " " "X" (Free (Final a b _)))) = readRawPoint a b
bonus (Free (Normal " " "X" (Free (Normal a b _)))) = readRawPoint a b
-- Spare Bonus
bonus (Free (Normal _ "/" (Free (Normal " " "X" _)))) = markedRawPoint
bonus (Free (Normal _ "/" (Free (Normal a _ _)))) = readRoll a
bonus (Free (Normal _ "/" (Free (Final a _ _)))) = readRoll a
-- otherwise
bonus _ = 0

runCalcScore :: Bowling -> Int
runCalcScore (Pure _) = 0
runCalcScore game@(Free (Normal a b next)) = readRawPoint a b + bonus game + runCalcScore next
runCalcScore (Free (Final a b c)) = readRawPointAt10Th a b c