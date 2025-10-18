{-# LANGUAGE DeriveFunctor #-}
module Main where

import Control.Monad.Free  

data Frame next = 
    Normal String String next
    | Final String String String
    deriving (Functor) 

type Bowling = Free Frame ()

frame :: String -> String -> Bowling
frame a b = liftF $ Normal a b ()

finalFrame :: String -> String -> String -> Bowling
finalFrame a b c = liftF $ Final a b c

frames :: Bowling
frames = do
    frame "9" "-"
    frame " " "X"
    frame "5" "4"
    frame " " "X"
    frame "8" "/"
    frame " " "X"
    frame " " "X"
    frame " " "X"
    frame "7" "/"
    finalFrame "X" "X" "9"

runPrintFrames :: Bowling -> IO ()
runPrintFrames (Pure _) = putStrLn "Game End"
runPrintFrames (Free (Normal a b next)) = do
    putStrLn $ "(" ++ a ++ "," ++ b ++ ")"
    runPrintFrames next
runPrintFrames (Free (Final a b c)) = do 
    putStrLn $ "(" ++ a ++ "," ++ b ++ "," ++ c ++ ")"

readRoll :: String -> Int
readRoll "X" = 10
readRoll "-" = 0
readRoll "/" = 0
readRoll "G" = 0
readRoll " " = 0
readRoll a = read a :: Int

readRawPoint :: String -> String -> Int
readRawPoint _ "X" = 10
readRawPoint _ "/" = 10
readRawPoint a b = readRoll a + readRoll b

readRawPointAt10Th :: String -> String -> String -> Int
readRawPointAt10Th "X" "X" r = 20 + readRoll r
readRawPointAt10Th "X" a b = 10 + readRawPoint a b
readRawPointAt10Th _ "/" b = 10 + readRoll b
readRawPointAt10Th a b _ = readRawPoint a b

bonus :: Bowling -> Int
-- Strike Bonus
bonus (Free (Normal " " "X" (Free (Normal " " "X" (Free (Normal " " "X" _)))))) = 20
bonus (Free (Normal " " "X" (Free (Normal " " "X" (Free (Normal a _ _)))))) = 10 + readRoll a
bonus (Free (Normal " " "X" (Free (Normal " " "X" (Free (Final a _ _)))))) = 10 + readRoll a
bonus (Free (Normal " " "X" (Free (Final a b _)))) = readRawPoint a b
bonus (Free (Normal " " "X" (Free (Normal a b _)))) = readRawPoint a b
-- Spare Bonus
bonus (Free (Normal _ "/" (Free (Normal " " "X" _)))) = 10
bonus (Free (Normal _ "/" (Free (Normal a _ _)))) = readRoll a
bonus (Free (Normal _ "/" (Free (Final a _ _)))) = readRoll a
-- otherwise
bonus _ = 0

calcScore :: Bowling -> Int
calcScore (Pure _) = 0
calcScore game@(Free (Normal a b next)) = readRawPoint a b + bonus game + calcScore next
calcScore (Free (Final a b c)) = readRawPointAt10Th a b c

main :: IO ()
main = do
    -- 1ゲームのフレームを表示
    runPrintFrames frames
    -- 1ゲームのスコアを表示
    print $ calcScore frames
