{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Scrollable
  ( ScrollableTextWindowed(..)
  , scrollableLayout
  , scrollableTextWindowed
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

truncateTop :: forall t m a. (Reflex t, Monad m, MonadNodeId m) => Int -> VtyWidget t m a -> VtyWidget t m a
truncateTop n w = do
  dw <- displayWidth
  dh <- displayHeight
  (result, imageB) <- lift (runVtyWidget' dw dh w)
  tellImages (fmap (fmap (cutTop n)) imageB)
  pure result


scrollableLayout
  :: forall t m a. (MonadHold t m, MonadFix m, Reflex t, MonadNodeId m, PostBuild t m)
  => Event t Int
  -> Behavior t V.Image
  -> VtyWidget t m (Dynamic t ())
scrollableLayout scrollBy img = do
  dw <- displayWidth
  dh <- displayHeight
  --((a, sz), imgs) <- lift $ runVtyWidget' dw dh w
  let sz = V.imageHeight <$> img
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
    attachPromptlyDyn dh $ attach sz requestedScroll
  bumpTop <-
    foldDynMaybe
      (\(ix, reqScroll) _ -> if ix == 0 && reqScroll < 0 then Just () else Nothing)
      ()
      (attach (current lineIndex) requestedScroll)
  tellImages $ (:[]) <$> ffor2 (current lineIndex) img cutTop
  display sz
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
