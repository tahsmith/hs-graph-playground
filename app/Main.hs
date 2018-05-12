module Main where

import Lib

g = Lib.Graph [[1], [2], [0]]

main :: IO ()
main = putStr $ show $ bfs g 0