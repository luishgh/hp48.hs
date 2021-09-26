{-|
Module      : Main
Description : Main entry for hp48.hs
Copyright   : (c) Luis Higino, 2021
License     : GPL-3
-}

module Main where

import HP48.Tui
import HP48.Stack (emptyStack)

main :: IO ()
main = tui emptyStack -- start tui with an empty stack
