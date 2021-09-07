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
handleInput :: S.HP48Stack -> String -> S.HP48Stack
handleInput stack input
  | inputReads /= [] =
    case inputReads of
      [(x, "")] -> S.push stack x
      _ -> stack
  where
    inputReads = reads input :: [(Double, String)]
handleInput stack "+" = S.applyBinaryFunction (+) stack
handleInput stack "-" = S.applyBinaryFunction (-) stack
handleInput stack "*" = S.applyBinaryFunction (*) stack
handleInput stack "x" = S.applyBinaryFunction (*) stack
handleInput stack "/" = S.applyBinaryFunction (/) stack
handleInput stack "**" = S.applyBinaryFunction (**) stack
handleInput stack "^" = S.applyBinaryFunction (**) stack
handleInput stack "ln" = S.applyUnaryFunction log stack
handleInput stack "sum"
  | S.isEmpty stack = stack
  | otherwise = S.singletonStack $ Just $ sum stack
handleInput stack "pop" = snd $ S.pop stack
handleInput stack _ = stack -- Default operation: does nothing
