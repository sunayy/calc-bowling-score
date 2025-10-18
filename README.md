# calc-score

ボウリングのスコアを計算するHaskellサンプルです。`Free` モナドでフレーム列を表現し、ストライク/スペアのボーナスを含めた合計スコアを算出します。`DeriveFunctor` を用いた `Functor` 導出の例も含みます。

## 構成
- 実行ファイル: `calc-score`
- エントリポイント: `app/Main.hs`
- 主要依存: `base`, `free`
- 拡張機能: `DeriveFunctor`

## ビルド・実行
以下は `cabal` を使った方法です。

- ビルド: `cabal build`
- 実行: `cabal run calc-score`

実行すると、サンプルゲームの各フレームと合計スコアが出力されます。

## プログラム概要
`Main.hs` では、ボウリングの 10 フレーム（10フレーム目は最大3投）を次のデータ型で表現しています。

```haskell
-- DeriveFunctor を使用
{-# LANGUAGE DeriveFunctor #-}

data Frame next
  = Normal String String next  -- 通常フレーム: 2投 + 続き
  | Final  String String String -- 第10フレーム: 最大3投
  deriving (Functor)

type Bowling = Free Frame ()
```

- `Normal a b next`: 1〜9フレームを表現。`a` と `b` は投球の記号（後述）。
- `Final a b c`: 10フレームを表現。3投目は必要な場合のみ使用。
- `frames`: サンプルゲーム（コード内でハードコード）
- `calcScore`: フレームごとの素点とボーナスを合計して、ゲーム全体のスコアを返す関数

### 記号の意味（`readRoll`）
- `"X"` = ストライク（10）
- `"/"` = スペア（後述の素点で10として扱う）
- `"-"` / `"G"` / `" "`（半角スペース） = 0
- 数字文字列（`"0".."9"`）= その数値

### 素点の扱い（`readRawPoint` / `readRawPointAt10Th`）
- 通常フレーム（1〜9）
  - 2投目が `X` または `/` の場合は素点を 10 とする
  - それ以外は 1投目 + 2投目
- 10フレーム（`Final`）
  - `X X r` は `20 + readRoll r`
  - `X a b` は `10 + readRawPoint a b`
  - `a / b` は `10 + readRoll b`
  - それ以外は `readRawPoint a b`

### ボーナス計算（`bonus`）
- ストライクのとき
  - 次が通常フレームなら、そのフレームの素点（次の2投）を加算
  - 連続ストライク（2連以上）のときは、次の次のフレームの1投目も考慮
  - 次が10フレームの場合も同様に先頭投球を参照
- スペアのとき
  - 次フレームの1投目（ストライクなら10）を加算

### 出力
`main` では、サンプルゲームの各フレームと合計スコアを表示します。

```haskell
main :: IO ()
main = do
  runPrintFrames frames      -- 各フレームの表示
  print $ calcScore frames   -- 合計スコアの表示
```

## サンプルゲーム
`frames` は次の通りです（`Main.hs` より）。

```haskell
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
```

実行例（概形）:

```
(9,-)
( ,X)
(5,4)
( ,X)
(8,/)
( ,X)
( ,X)
( ,X)
(7,/)
(X,X,9)
203
```

※ スコアは上記実装ロジックに基づく計算結果です。

## カスタマイズ
- 別のスコアを試したい場合は、`Main.hs` の `frames` を編集してください。
- フレームの生成は `frame a b`（通常フレーム）、`finalFrame a b c`（10フレーム）で行います。
- 記号は前述の仕様（`readRoll`）に従ってください。

## 開発メモ
- `DeriveFunctor` を使って `Frame` を自動で `Functor` にしておき、`Free Frame` としてゲームを構築しています。
- 連続ストライクや10フレームの特例は、`bonus` と `readRawPointAt10Th` のパターンマッチで扱っています。