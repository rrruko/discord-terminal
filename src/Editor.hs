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
    let network = ffor editorState (\es -> editorNetwork es users)
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
  -> Dynamic t (M.Map T.Text UserId)
  -> VtyWidget t m (Event t EditorCommand)
editorNetwork s users = case state s of
  Mention -> mentionWidget users
  Edit -> editWidget (content s)

mentionWidget
  :: forall t m. EditorConstraints t m
  => Dynamic t (M.Map T.Text UserId)
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
    mention = fmapMaybe (uncurry (M.!?)) (attach (current users) mentionedUser)
  pure
    (leftmost
      [ SubmitMention <$> mention
      , QuitMention <$ quit
      ])

userNameChoices
  :: forall t m. EditorConstraints t m
  => Dynamic t T.Text
  -> Dynamic t (M.Map T.Text UserId)
  -> VtyWidget t m ()
userNameChoices prefix names =
  text $ current $
    ffor2 prefix names (\p n ->
      let
        nameList = M.keys n
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
    textInput' def
      { _textInputConfig_initialValue = fromText initValue
      }
  pure
    (leftmost
      [ BeginMention <$> tag (current (_textInput_value textInp)) mention
      , SubmitPost <$> tag (current (_textInput_value textInp)) submitPost
      ])

-- This is just the reflex-vty textInput widget with tab inputs filtered out.
-- I'm using tab for navigation and I don't want to worry about keeping my
-- input widget unfocused when the user is trying to tab out.
textInput'
  :: (Reflex t, MonadHold t m, MonadFix m)
  => TextInputConfig t
  -> VtyWidget t m (TextInput t)
textInput' cfg = do
  i <- input <&> ffilter (\ev -> ev /= V.EvKey (V.KChar '\t') [])
  f <- focus
  dh <- displayHeight
  dw <- displayWidth
  rec v <- foldDyn ($) (_textInputConfig_initialValue cfg) $ mergeWith (.)
        [ uncurry (updateTextZipper (_textInputConfig_tabWidth cfg)) <$> attach (current dh) i
        , _textInputConfig_modify cfg
        , let displayInfo = (,) <$> current rows <*> scrollTop
          in ffor (attach displayInfo click) $ \((dl, st), MouseDown _ (mx, my) _) ->
            goToDisplayLinePosition mx (st + my) dl
        ]
      click <- mouseDown V.BLeft
      let cursorAttrs = ffor f $ \x -> if x then cursorAttributes else V.defAttr
      let rows = (\w s c -> displayLines w V.defAttr c s)
            <$> dw
            <*> (mapZipper <$> _textInputConfig_display cfg <*> v)
            <*> cursorAttrs
          img = images . _displayLines_spans <$> rows
      y <- holdUniqDyn $ _displayLines_cursorY <$> rows
      let newScrollTop :: Int -> (Int, Int) -> Int
          newScrollTop st (h, cursorY)
            | cursorY < st = cursorY
            | cursorY >= st + h = cursorY - h + 1
            | otherwise = st
      let hy = attachWith newScrollTop scrollTop $ updated $ zipDyn dh y
      scrollTop <- hold 0 hy
      tellImages $ (\imgs st -> (:[]) . V.vertCat $ drop st imgs) <$> current img <*> scrollTop
  return $ TextInput
    { _textInput_value = value <$> v
    , _textInput_lines = length . _displayLines_spans <$> rows
    }
