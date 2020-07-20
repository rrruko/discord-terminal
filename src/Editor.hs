{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Editor
  ( editor
  ) where

import Control.Lens.Operators
import Control.Monad.Fix
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text.Zipper
import Discord.Types hiding (Event)
import Reflex
import Reflex.Network
import Reflex.Vty
import Reflex.Vty.Widget
import Reflex.Vty.Widget.Input
import qualified Graphics.Vty as V

data EditorMode
  = Edit
  | Mention

data EditorState = EditorState
  { state :: EditorMode
  , content :: T.Text
  }

data EditorCommand
  = SubmitMention UserId
  | QuitMention
  | BeginMention T.Text
  | SubmitPost T.Text

type EditorConstraints t m =
  ( Reflex t
  , MonadHold t m
  , MonadFix m
  , MonadNodeId m
  , NotReady t m
  , Adjustable t m
  , PostBuild t m
  )

editor
  :: forall t m. EditorConstraints t m
  => Dynamic t (M.Map T.Text UserId)
  -> VtyWidget t m (Event t T.Text)
editor users = mdo
    let network = ffor2 editorState users editorNetwork
    editorUpdate <- switchHold never =<< networkView network
    editorState <- foldDyn updateEditor initialEditorState editorUpdate
    pure (fforMaybe editorUpdate
      \case
        SubmitPost t -> Just t
        _ -> Nothing)
  where
  initialEditorState = EditorState Edit mempty
  updateEditor x (EditorState s c) =
    case x of
      BeginMention persistedContent -> EditorState Mention persistedContent
      SubmitMention mention -> EditorState Edit (c <> formatMention mention)
      SubmitPost post -> EditorState Edit mempty
      QuitMention -> EditorState Edit c
  formatMention ident = "<@" <> T.pack (show ident) <> ">"

editorNetwork
  :: forall t m. EditorConstraints t m
  => EditorState
  -> M.Map T.Text UserId
  -> VtyWidget t m (Event t EditorCommand)
editorNetwork s users = case state s of
  Mention -> mentionWidget users
  Edit -> editWidget (content s)

mentionWidget
  :: forall t m. EditorConstraints t m
  => M.Map T.Text UserId
  -> VtyWidget t m (Event t EditorCommand)
mentionWidget users = do
  submitMention <- key V.KEnter
  quit <- keyCombo (V.KChar 'a', [V.MCtrl])
  rec
    textInp <- runLayout (constDyn Orientation_Column) 1 never $ do
      fixed 1 (userNameChoices (_textInput_value textInp) users)
      fixed 1 (textInput def)
  let
    mentionedUser = tag (current (_textInput_value textInp)) submitMention
    mention = fmapMaybe (\name -> users M.!? name) mentionedUser
  pure
    (leftmost
      [ SubmitMention <$> mention
      , QuitMention <$ quit
      ])

userNameChoices
  :: forall t m. EditorConstraints t m
  => Dynamic t T.Text
  -> M.Map T.Text UserId
  -> VtyWidget t m ()
userNameChoices prefix names =
  text $ current $
    ffor prefix (\p ->
      let
        nameList = M.keys names
        choices = filter (p `T.isPrefixOf`) nameList
      in
        (T.intercalate ", " choices))

editWidget
  :: forall t m. EditorConstraints t m
  => T.Text
  -> VtyWidget t m (Event t EditorCommand)
editWidget initValue = do
  submitPost <- key V.KEnter
  mention <- fmap (Mention <$) (keyCombo (V.KChar 'a', [V.MCtrl]))
  textInp <-
    textInput def
      { _textInputConfig_initialValue = fromText initValue
      }
  pure
    (leftmost
      [ BeginMention <$> tag (current (_textInput_value textInp)) mention
      , SubmitPost <$> tag (current (_textInput_value textInp)) submitPost
      ])
