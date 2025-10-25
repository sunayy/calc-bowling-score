module Main where

import Control.Monad.Free  
import ByFree as F   
import ByList as L  

frames :: F.Bowling
frames = do
    F.frame "9" "-"
    F.frame " " "X"
    F.frame "5" "4"
    F.frame " " "X"
    F.frame "8" "/"
    F.frame " " "X"
    F.frame " " "X"
    F.frame " " "X"
    F.frame "7" "/"
    F.finalFrame "X" "X" "9"

framesList :: L.Bowling
framesList = [
    L.frame "9" "-",
    L.frame " " "X",
    L.frame "5" "4",
    L.frame " " "X",
    L.frame "8" "/",
    L.frame " " "X",
    L.frame " " "X",
    L.frame " " "X",
    L.frame "7" "/",
    L.finalFrame "X" "X" "9"
    ]

main :: IO ()
main = do
    putStrLn "By Free" 
    -- 1ゲームのフレームを表示
    F.runPrintFrames frames
    -- 1ゲームのスコアを表示
    print $ F.runCalcScore frames

    putStrLn "By List"
    -- 1ゲームのスコアを表示
    print $ L.calcSum framesList

