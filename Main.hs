{-|
Module      : Main
Description : Main entry for hp48.hs
Copyright   : (c) Luis Higino, 2021
License     : GPL-3
-}

module Main where

import Control.Monad
import HP48.Controller
import HP48.IO
import qualified HP48.Stack as S

main :: IO ()
main = calculator S.emptyStack

-- | Prints menu, gets menu input and calculates new stack if input isn't @"e"@.
calculator :: S.HP48Stack -> IO ()
calculator stack = do
  printMenu stack
  input <- getMenuInput
  when (input /= "e") $ do
    let newStack = handleInput stack input
    calculator newStack
