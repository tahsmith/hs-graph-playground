module Main where

import Lib

g = Graph [
    [1, 2],
    [3],
    [4],
    [],
    [],
    []]

main :: IO ()
main = putStr $ show $ bfs g 0