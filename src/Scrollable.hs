{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Scrollable
  ( ScrollableTextWindowed(..)
  , scrollableTextWindowed
  ) where

import Control.Monad.Fix
import Data.Text (Text)
import qualified Data.Text as T
import Reflex
import Reflex.Vty
import Data.Text.Zipper
import qualified Graphics.Vty as V

data ScrollableTextWindowed t = ScrollableTextWindowed
  { scrollableTextWindowed_position :: Behavior t (Int, Int, Int)
    -- ^ Triple of the first shown line, last shown line, and the line count
  , scrollableTextWindowed_bumpTop :: Event t ()
    -- ^ Fires when the user tries to scroll past the top of the window
  }

scrollableTextWindowed
  :: forall t m. (MonadHold t m, MonadFix m, Reflex t, MonadNodeId m)
  => Event t Int
  -> Dynamic t [Text]
  -> VtyWidget t m (ScrollableTextWindowed t)
scrollableTextWindowed scrollBy contents = mdo
  f <- focus
  aw <- displayWidth
  ah <- displayHeight
  (pos, len, bump) <- pane
    (widgetRegion aw ah len)
    f
    (scrollableText' scrollBy contents)
  pure (ScrollableTextWindowed pos bump)
  where
  widgetRegion aw ah widgetResult =
    DynRegion
      (constDyn 0)
      ((\x y -> max 0 (x - y)) <$> ah <*> widgetResult)
      aw
      (min <$> widgetResult <*> ah)

scrollableText'
  :: forall t m. (MonadHold t m, MonadFix m, Reflex t, MonadNodeId m)
  => Event t Int
  -> Dynamic t [Text]
  -> VtyWidget t m (Behavior t (Int, Int, Int), Dynamic t Int, Event t ())
scrollableText' scrollBy contents = do
  dw <- displayWidth
  dh <- displayHeight
  let imgs = wrap <$> dw <*> (T.intercalate "\n" <$> contents)
  kup <- key V.KUp
  kdown <- key V.KDown
  m <- mouseScroll
  let requestedScroll :: Event t Int
      requestedScroll = leftmost
        [ 1 <$ kdown
        , (-1) <$ kup
        , ffor m $ \case
            ScrollDirection_Up -> (-1)
            ScrollDirection_Down -> 1
        , scrollBy
        ]
      updateLine maxN delta ix h = max 0 (min (maxN - h) (ix + delta))
  lineIndex :: Dynamic t Int <- foldDyn (\(h, (maxN, delta)) ix -> updateLine maxN delta ix h) 0 $
    attachPromptlyDyn dh $ attachPromptlyDyn (length <$> imgs) requestedScroll
  bumpTop <-
    foldDynMaybe
      (\(ix, reqScroll) _ -> if ix == 0 && reqScroll < 0 then Just () else Nothing)
      ()
      (attach (current lineIndex) requestedScroll)
  tellImages $ fmap ((:[]) . V.vertCat) $ current $ drop <$> lineIndex <*> imgs
  let result = (,,)
        <$> ((+) <$> current lineIndex <*> pure 1)
        <*> ((+) <$> current lineIndex <*> current dh)
        <*> (length <$> current imgs)
  pure (result, length <$> imgs, updated bumpTop)
  where
  wrap maxWidth =
    concatMap (fmap (V.string V.defAttr . T.unpack) . wrapWithOffset maxWidth 0) .
    T.split (=='\n')
