{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Test.Hspec

import qualified DiscordVty
import qualified Scrollable
import Data.Bool
import Data.MemoTrie
import Control.Monad.Identity
import Unsafe.Coerce
import Control.Monad.Fix
import Control.Monad.Reader
import Control.Monad.State.Strict
import Reflex
import Reflex.Class
import Reflex.Pure
import Reflex.Vty
import qualified Graphics.Vty as V
import qualified Data.Text as T

runWidgetPure
  :: (t ~ Pure Int)
  => VtyWidgetCtx t
  -> VtyWidget t (StateT Integer ((->) Int)) a
  -> a
runWidgetPure ctx = fst . fst . ($ 0) . flip runStateT 0 . runVtyWidget ctx

-- unsafeCoerce is used here because NodeId
-- does not export its constructor
instance (m ~ (->) a, s ~ Integer) => MonadNodeId (StateT s m) where
  getNextNodeId = do
    newId <- get
    modify (+1)
    pure (unsafeCoerce newId)

instance (HasTrie t, Enum t, Ord t) => PostBuild (Pure t) ((->) t) where
  getPostBuild t = Event (\t' -> if t == t' then Just () else Nothing)

defaultCtx :: (Reflex t) => VtyWidgetCtx t
defaultCtx = VtyWidgetCtx
  (constDyn 80)
  (constDyn 40)
  (constDyn True)
  never

type PureVtyWidget a = VtyWidget (Pure Int) (StateT Integer ((->) Int)) a

spec :: SpecWith ()
spec = do
  describe "truncateTop" $ do
    let
      w :: PureVtyWidget ()
      w = col $ do
        fixed 1 $ text (pure "A")
        fixed 1 $ text (pure "BC")
        fixed 1 $ text (pure "DEF")
      getImageB :: PureVtyWidget a -> Int -> [V.Image]
      getImageB widget =
        let ((_, imageB), _) = runStateT (runVtyWidget defaultCtx widget) 0 0
        in  unBehavior imageB

    it "doesn't alter images if 0 is passed" $ do
      getImageB (truncateTop 0 w) 0 `shouldBe` getImageB w 0

    it "removes the first line when 1 is passed" $ do
      (getImageB (truncateTop 1 w) 0 !! 0) `shouldBe` V.emptyImage
      (getImageB (truncateTop 1 w) 0 !! 1) `shouldNotBe` V.emptyImage
      (getImageB (truncateTop 1 w) 0 !! 2) `shouldNotBe` V.emptyImage

    it "removes everything when 3 is passed" $ do
      getImageB (truncateTop 3 w) 0 `shouldBe` [V.emptyImage, V.emptyImage, V.emptyImage]

  describe "scrollableTextWindowed" $ do
    it "stays within bounds" $ do
      let
        inp = Event $ \t -> Just (V.EvKey V.KDown [])
        widgetContents = pure (T.lines "A\nB\nC\nD\nE")
        vtyCtx windowHeight = defaultCtx
          { _vtyWidgetCtx_height = constDyn windowHeight
          , _vtyWidgetCtx_input = inp
          }
        widget windowHeight =
          runWidgetPure
            (vtyCtx windowHeight)
            (Scrollable.scrollableTextWindowed never widgetContents)
      unBehavior (Scrollable.scrollableTextWindowed_position $ widget 3) 0 `shouldBe` (1,3,5)
      unBehavior (Scrollable.scrollableTextWindowed_position $ widget 3) 1 `shouldBe` (2,4,5)
      unBehavior (Scrollable.scrollableTextWindowed_position $ widget 3) 2 `shouldBe` (3,5,5)
      unBehavior (Scrollable.scrollableTextWindowed_position $ widget 3) 100 `shouldBe` (3,5,5)
      unBehavior (Scrollable.scrollableTextWindowed_position $ widget 100) 0 `shouldBe` (1,5,5)

main :: IO ()
main = hspec spec
