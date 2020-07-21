{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Scrollable
  ( scrollableTextWindowed
  ) where

import Control.Monad.Fix
import Data.Text (Text)
import qualified Data.Text as T
import Reflex
import Reflex.Vty
import Data.Text.Zipper
import qualified Graphics.Vty as V

scrollableTextWindowed
  :: forall t m. (MonadHold t m, MonadFix m, Reflex t, MonadNodeId m)
  => Event t Int
  -> Dynamic t [Text]
  -> VtyWidget t m (Behavior t (Int, Int, Int))
scrollableTextWindowed scrollBy contents = mdo
  f <- focus
  aw <- displayWidth
  ah <- displayHeight
  result <- pane
    (widgetRegion aw ah (snd result))
    f
    (scrollableText' scrollBy contents)
  pure (fst result)
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
  -> VtyWidget t m (Behavior t (Int, Int, Int), Dynamic t Int)
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
  tellImages $ fmap ((:[]) . V.vertCat) $ current $ drop <$> lineIndex <*> imgs
  let result = (,,)
        <$> ((+) <$> current lineIndex <*> pure 1)
        <*> ((+) <$> current lineIndex <*> current dh)
        <*> (length <$> current imgs)
  pure (result, length <$> imgs)
  where
  wrap maxWidth =
    concatMap (fmap (V.string V.defAttr . T.unpack) . wrapWithOffset maxWidth 0) .
    T.split (=='\n')
