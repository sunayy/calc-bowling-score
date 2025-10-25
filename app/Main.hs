module Main where

import Control.Monad.Free  
import ByFree   

-- Uses Free Monad to represent Bowling Game
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

main :: IO ()
main = do
    -- 1ゲームのフレームを表示
    runPrintFrames frames
    -- 1ゲームのスコアを表示
    print $ runCalcScore frames
