{-|
Module      : HP48.IO
Description : IO functions to compose a UI
Copyright   : (c) Luis Higino, 2021
License     : GPL-3
-}

module HP48.IO
  ( printMenu
  , getMenuInput
  ) where

import qualified HP48.Stack as S
import System.IO

-- | Prints a @stack@ member on 'stdout'.
printStackMember :: S.HP48Stack -> Int -> IO ()
printStackMember stack idx =
  putStrLn $ show idx ++ ": \t\t\t\t\t\t" ++ (S.show . S.getValue stack) idx

-- | Prints @stack@ on 'stdout'.
printStack :: S.HP48Stack -> IO ()
printStack stack = mapM_ (printStackMember stack) (reverse [1 .. 4])

-- | Prints menu on 'stdout'.
printMenu :: S.HP48Stack -> IO ()
printMenu stack = do
  putStrLn "{ HOME } "
  putStrLn "===================================================="
  printStack stack

-- | Gets menu input on 'stdin'.
getMenuInput :: IO String
getMenuInput = do
  putStr "> "
  hFlush stdout
  getLine
