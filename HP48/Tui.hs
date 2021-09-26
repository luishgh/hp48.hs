{-|
Module      : HP48.Tui
Description : Tui for application using Brick
Copyright   : (c) Luis Higino, 2021
License     : GPL-3
-}

{-# LANGUAGE TemplateHaskell #-}

module HP48.Tui
  ( tui
  ) where

import Lens.Micro.Platform

import Data.Text.Zipper (clearZipper)

import Brick.Main
import Brick.AttrMap
import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.Border
import Brick.Widgets.Edit
import Graphics.Vty.Input.Events

import HP48.Stack (HP48Stack)
import qualified HP48.Stack as S
import HP48.Controller

-- Use strings as resource names, not really used at the moment
type ResourceName = String

-- | The type that defines the state structure used during runtime
data TuiState =
  TuiState
    { _stack :: HP48Stack -- | Current stack
    , _header :: String -- | Current header content
    , _input :: Editor String ResourceName -- | Current user input
    }
  deriving (Show)
makeLenses ''TuiState

-- | Widget that represents current state of a stack member
stackMemberWidget :: HP48Stack -> Int -> Widget ResourceName
stackMemberWidget stack idx =
  padRight Max (str $ show idx ++ ":") <+> -- member number
  (padLeft Max . str . S.show . S.getValue stack) idx -- member value

-- | Widget that represents current state of stack
stackWidget :: HP48Stack -> Widget ResourceName
stackWidget stack = vBox $ map (stackMemberWidget stack) (reverse [1 .. 4])

-- | Unicode arrow used on the user prompt.
arrowChar :: String
arrowChar = "\x1F840"

-- | Function that draws a tui based on current state
drawTui :: TuiState -> [Widget ResourceName]
drawTui ts =
  [ vBox
      [ str $ ts ^. header -- header
      , hBorder
      , stackWidget $ ts ^. stack -- stack
      , renderEditor -- user input
          (str . unlines . map (++ (" " ++ arrowChar)))
          True
          (ts ^. input)
      ]
  ]

-- | Handles events, returning the next state
handleTuiEvent ::
     TuiState -- | Current state
  -> BrickEvent ResourceName e -- | Event that needs to be handled
  -> EventM ResourceName (Next TuiState)
handleTuiEvent ts (VtyEvent ev) =
  case ev of
    EvKey KEsc [] -> halt ts -- quit
    EvKey (KChar 'q') [] -> halt ts -- quit
    EvKey KEnter [] -- enter input
     ->
      let currentInput = head . getEditContents . view input $ ts -- current user input
          updateStack = stack %~ handleInput currentInput -- extracts current stack and updates it
          clearInput = input %~ applyEdit clearZipper -- clears user input
       in continue $ ts & updateStack & clearInput
    _ -> continue =<< handleEventLensed ts input handleEditorEvent ev -- update input content

-- | Definition of the tui elements
tuiApp :: App TuiState e ResourceName
tuiApp =
  App
    { appDraw = drawTui
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleTuiEvent
    , appStartEvent = pure
    , appAttrMap = const $ attrMap mempty []
    }

-- | The tui application
tui ::
    HP48Stack -- | Initial stack
    -> IO ()
tui stack = do
  let initialState = TuiState stack "{ HOME }" (editor "Input" Nothing "")
  defaultMain tuiApp initialState
  return ()
