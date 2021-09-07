{-|
Module      : HP48.Stack
Description : A simple stack implementation
Copyright   : (c) Luis Higino, 2021
License     : GPL-3
-}

module HP48.Stack
  ( Stack
  , HP48Stack
  , HP48.Stack.show
  , createStack
  , emptyStack
  , singletonStack
  , isEmpty
  , push
  , pop
  , getValue
  , applyUnaryFunction
  , applyBinaryFunction
  ) where

import Data.Maybe
import Text.Printf (printf)

-- | Returns the value inside Just or an empty string.
-- Used on representations of 'Stack'
show :: (Show a) => Maybe a -> String
show (Just value) = Prelude.show value
show Nothing = ""

-- | Simple 4 element stack.
data Stack a =
  Stack (Maybe a) (Maybe a) (Maybe a) (Maybe a)

-- | The concrete type used on another modules.
type HP48Stack = Stack Double

instance Foldable Stack where
  foldr f n (Stack fst' snd' trd fth) =
    let justs = [fromJust x | x <- [fst', snd', trd, fth], isJust x] -- filter Just values
     in foldr f n justs

instance (Show a) => Show (Stack a) where
  show (Stack fst' snd' trd fth) =
    printf
      "t [%s][%s][%s][%s] b"
      (HP48.Stack.show fst')
      (HP48.Stack.show snd')
      (HP48.Stack.show trd)
      (HP48.Stack.show fth)

-- | Returns whether the stack is empty.
isEmpty :: Stack a -> Bool
isEmpty (Stack Nothing _ _ _) = True
isEmpty _ = False

-- | Applies unary function @f@ to first element of @s@.
applyUnaryFunction :: (a -> a) -> Stack a -> Stack a
applyUnaryFunction f s@(Stack fst' _ _ _) = insert' s (f <$> fst')

-- | Applies binary function @f@ to first two elements of @s@, inserting the result in place of them.
applyBinaryFunction :: (a -> a -> a) -> Stack a -> Stack a
applyBinaryFunction f s@(Stack fst' snd' _ _) = insert' s (f <$> snd' <*> fst')

-- | Returns a new 'Stack' composed of passed values.
createStack :: Maybe a -> Maybe a -> Maybe a -> Maybe a -> Stack a
createStack = Stack

-- | Creates 'Stack' composed of single value.
singletonStack :: Maybe a -> Stack a
singletonStack v = Stack v Nothing Nothing Nothing

-- | Creates empty 'Stack'.
emptyStack :: Stack a
emptyStack = Stack Nothing Nothing Nothing Nothing

-- | Inserts value at the top of the 'Stack'.
push :: Stack a -> a -> Stack a
push (Stack fst' snd' trd _) value = Stack (Just value) fst' snd' trd

-- | Removes value at the top of the 'Stack'
pop :: Stack a -> (Maybe a, Stack a)
pop (Stack fst' snd' trd fth) = (fst', Stack snd' trd fth Nothing)

-- | Gets value at position in 'Stack'.
getValue :: Stack a -> Int -> Maybe a
getValue (Stack val _ _ _) 1 = val
getValue (Stack _ val _ _) 2 = val
getValue (Stack _ _ val _) 3 = val
getValue (Stack _ _ _ val) 4 = val
getValue _ num =
  error $
  "Stack only has four members, but getValue was called with: " ++
  Prelude.show num

-- Used only on internal functions
-- When value is Just, insert it in the first position and throw second value away
-- Otherwise, does nothing
insert' :: Stack a -> Maybe a -> Stack a
insert' (Stack _ _ trd fth) (Just value) = Stack (Just value) trd fth Nothing
insert' stack Nothing = stack
