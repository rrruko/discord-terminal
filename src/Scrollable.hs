{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Scrollable
  ( ScrollableTextWindowed(..)
  , scrollableImage
  , scrollableImageWindowed
  , scrollableTextWindowed
  , truncateTop
  ) where

import Control.Monad.Trans.Class
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

truncateTop
  :: forall t m a. (Reflex t, Monad m, MonadNodeId m)
  => Int
  -> VtyWidget t m a
  -> VtyWidget t m a
truncateTop n w = do
  dw <- displayWidth
  dh <- displayHeight
  (result, imageB) <- lift (runVtyWidget' dw dh w)
  tellImages (fmap (fmap (cutTop n)) imageB)
  pure result

-- Remove up to n lines from the top of an image
cutTop :: Int -> V.Image -> V.Image
cutTop n im = V.cropTop (max 0 $ V.imageHeight im - n) im

runVtyWidget' :: (Reflex t, MonadNodeId m) => Dynamic t Int -> Dynamic t Int -> VtyWidget t m a -> m (a, Behavior t [V.Image])
runVtyWidget' dw dh w =
  runVtyWidget
    (VtyWidgetCtx
      { _vtyWidgetCtx_width = dw
      , _vtyWidgetCtx_height = dh
      , _vtyWidgetCtx_focus = constDyn False
      , _vtyWidgetCtx_input = never
      })
    w

scrollableImageWindowed
  :: forall t m. (MonadHold t m, MonadFix m, Reflex t, MonadNodeId m, PostBuild t m)
  => Event t (Either Int ())
  -> Dynamic t V.Image
  -> VtyWidget t m (Dynamic t ())
scrollableImageWindowed scrollBy img = do
  dh <- displayHeight
  let size = fmap V.imageHeight img
  let margin = max <$> (dh - size) <*> 0
  snd <$> splitV
    (fmap const margin)
    (constDyn (False, False))
    blank
    (scrollableImage scrollBy img)

scrollableImage
  :: forall t m. (MonadHold t m, MonadFix m, Reflex t, MonadNodeId m, PostBuild t m)
  => Event t (Either Int ())
  -> Dynamic t V.Image
  -> VtyWidget t m (Dynamic t ())
scrollableImage scrollBy img = do
  dh <- displayHeight
  let size = fmap V.imageHeight img
  kup <- key V.KUp
  kdown <- key V.KDown
  m <- mouseScroll
  let requestedScroll :: Event t (Either Int ())
      requestedScroll = leftmost
        [ Left 1 <$ kdown
        , Left (-1) <$ kup
        , ffor m $ \case
            ScrollDirection_Up -> Left (-1)
            ScrollDirection_Down -> Left 1
        , scrollBy
        ]
      updateLine maxN delta ix h =
        case delta of
          Left n -> max 0 (min (maxN - h) (ix + n))
          Right () -> max 0 (maxN - h)
  lineIndex :: Dynamic t Int <-
    foldDyn (\(h, (maxN, delta)) ix -> updateLine maxN delta ix h) 0 $
      attachPromptlyDyn dh $ attachPromptlyDyn size requestedScroll
  bumpTop <-
    foldDynMaybe
      (\(ix, reqScroll) _ ->
        if ix == 0 && either (< 0) (const False) reqScroll
          then Just ()
          else Nothing)
      ()
      (attach (current lineIndex) requestedScroll)
  tellImages $ current $ (:[]) <$> ffor2 lineIndex img cutTop
  display (current size)
  pure bumpTop

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
