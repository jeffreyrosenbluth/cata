module Main where

import FixMuNu

main :: IO ()
main = putStrLn "Hello Cata"

maybeAlg :: Maybe Int -> Int
maybeAlg Nothing = 0
maybeAlg (Just a) = 1 + a

zeroMu :: Mu Maybe
zeroMu = inMu Nothing

succMu :: Mu Maybe -> Mu Maybe
succMu = inMu . Just

oneMu, twoMu :: Mu Maybe
oneMu = succMu zeroMu
twoMu = succMu oneMu

maybeMuToInt :: Mu Maybe -> Int
maybeMuToInt = cataMu maybeAlg