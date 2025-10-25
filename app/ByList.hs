module ByList (Bowling, calcSum, frame, finalFrame) where

data Frame = Normal (String, String) | Final (String, String, String) | Empty

type Bowling = [Frame]

frame :: String -> String -> Frame
frame a b = Normal (a, b)

finalFrame :: String -> String -> String -> Frame
finalFrame a b c = Final (a, b, c)

markedRawPoint :: Int
markedRawPoint = 10

readRoll :: String -> Int
readRoll "X" = markedRawPoint
readRoll "-" = 0
readRoll "/" = 0
readRoll "G" = 0
readRoll " " = 0
readRoll a = read a :: Int

calcSum :: Bowling -> Int
calcSum fs = foldr sumScore 0 zipped
    where
        -- 最大２フレーム前の情報が必要なので、２つ後のフレームを追加する
        zipped :: [(Frame, (Frame, Frame))]
        zipped =
            let next1  = drop 1 fs ++ [Empty]
                next2  = drop 2 fs ++ [Empty, Empty]
            in zip fs (zip next1 next2)

        sumScore :: (Frame, (Frame, Frame)) -> Int -> Int
        -- 最終フレームの場合は、２つ後のフレームの情報は不要
        sumScore (Final (_, "/", a), (_, _)) acc = acc + markedRawPoint + readRoll a
        sumScore (Final ("X", _, "/"), (_, _)) acc = acc + markedRawPoint * 2
        sumScore (Final (a, b, c), (_, _)) acc = acc + sum (map readRoll [a, b, c])

        sumScore (Normal (_, "X"), (Normal (_, "X"), Normal (_, "X"))) acc = acc + markedRawPoint * 3
        sumScore (Normal (_, "X"), (Normal (_, "X"), Normal (a, _))) acc = acc + markedRawPoint * 2 + readRoll a
        sumScore (Normal (_, "X"), (Normal (_, "X"), Final (a, _, _))) acc = acc + markedRawPoint * 2 + readRoll a

        sumScore (Normal (_, "X"), (Normal (_, "/"), _)) acc = acc + markedRawPoint * 2
        sumScore (Normal (_, "X"), (Normal (a, b), _)) acc = acc + markedRawPoint + sum (map readRoll [a, b])
        sumScore (Normal (_, "X"), (Final (a, b, _), _)) acc = acc + markedRawPoint + sum (map readRoll [a, b])

        sumScore (Normal (_, "/"), (Normal (_, "X"), _)) acc = acc + markedRawPoint * 2
        sumScore (Normal (_, "/"), (Normal (a, _), _)) acc = acc + markedRawPoint + readRoll a
        sumScore (Normal (_, "/"), (Final (a, _, _), _)) acc = acc + markedRawPoint + readRoll a

        sumScore (Normal (a, b), (_, _)) acc = acc + sum (map readRoll [a, b])        

