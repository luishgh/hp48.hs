{-|
Module      : HP48.Controller
Description : Handles input executing corresponding functions
Copyright   : (c) Luis Higino, 2021
License     : GPL-3
-}

module HP48.Controller
  ( handleInput
  ) where

import qualified HP48.Stack as S

-- | Handles @input@ executing the corresponding function.
-- If the required number of arguments is not present on the 'S.Stack', it fails silently.
-- The following inputs are supported at the moment:
--
-- +-------+----------------+--------------+
-- | Input | Operation      | N° arguments |
-- +=======+================+==============+
-- | +     | Addition       | 2            |
-- +-------+----------------+--------------+
-- | -     | Subtraction    | 2            |
-- +-------+----------------+--------------+
-- | *     | Multiplication | 2            |
-- +-------+----------------+--------------+
-- | x     | Multiplication | 2            |
-- +-------+----------------+--------------+
-- | /     | Division       | 2            |
-- +-------+----------------+--------------+
-- | **    | Exponentiation | 2            |
-- +-------+----------------+--------------+
-- | ^     | Exponentiation | 2            |
-- +-------+----------------+--------------+
-- | ln    | Logarithm      | 1            |
-- +-------+----------------+--------------+
handleInput :: String -> S.HP48Stack -> S.HP48Stack
handleInput input stack
  | inputReads /= [] =
    case inputReads of
      [(x, "")] -> S.push stack x
      _ -> stack
  where
    inputReads = reads input :: [(Double, String)]
handleInput "+" stack = S.applyBinaryFunction (+) stack
handleInput "-" stack = S.applyBinaryFunction (-) stack
handleInput "*" stack = S.applyBinaryFunction (*) stack
handleInput "x" stack = S.applyBinaryFunction (*) stack
handleInput "/" stack = S.applyBinaryFunction (/) stack
handleInput "**" stack = S.applyBinaryFunction (**) stack
handleInput "^" stack = S.applyBinaryFunction (**) stack
handleInput "ln" stack = S.applyUnaryFunction log stack
handleInput "sum" stack
  | S.isEmpty stack = stack
  | otherwise = S.singletonStack $ Just $ sum stack
handleInput "pop" stack = snd $ S.pop stack
handleInput _ stack = stack -- Default operation: does nothing
