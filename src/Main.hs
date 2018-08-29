module Main where

import Control.Monad.State
import Control.Monad.Free
import Criterion.Main
import Position

type Alonzo a = StateT Position IO a

engine :: Alonzo ()
engine = do
    get >>= liftIO . print
    modify ((!! 0) . evolve)
    get >>= liftIO . print

main :: IO ()
main = print $ perft 6

fake :: IO ()
fake = print (perft 1)
    >> print (perft 2)
    >> print (perft 3)
    >> print (perft 4)
    >> print (perft 5)
    >> defaultMain [
    bgroup "perft" [ bench "1" $ whnf perft 1
                   , bench "2" $ whnf perft 2
                   , bench "3" $ whnf perft 3
                   , bench "4" $ whnf perft 4
                   , bench "5" $ whnf perft 5
                   ]
    ]

perft :: Integer -> Int
perft n = length $ retract $ cutoff n $ unfold (Right . evolve) position
        where position = initialPosition --readFEN "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10"
