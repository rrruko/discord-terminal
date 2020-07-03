{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec

import qualified DiscordVty

import Data.Bool
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

defaultCtx :: (Reflex t) => VtyWidgetCtx t
defaultCtx = VtyWidgetCtx
  (constDyn 80)
  (constDyn 40)
  (constDyn True)
  never

spec :: SpecWith ()
spec = do
  describe "scrollableText'" $ do
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
            (DiscordVty.scrollableText' never widgetContents)
      unBehavior (widget 3) 0 `shouldBe` (1,3,5)
      unBehavior (widget 3) 1 `shouldBe` (2,4,5)
      unBehavior (widget 3) 2 `shouldBe` (3,5,5)
      unBehavior (widget 3) 100 `shouldBe` (3,5,5)
      unBehavior (widget 100) 0 `shouldBe` (1,5,5)

  describe "channelView" $ do
    it "returns the user's input" $ do
      let
        userKeys =
          [ V.EvKey (V.KChar c) [] | c <- "hello" ] <>
          [ V.EvKey V.KEnter [] ]
        inp = Event $ \t ->
          if t < length userKeys
            then Just (userKeys !! t)
            else Nothing
        widget =
          runWidgetPure
            (defaultCtx { _vtyWidgetCtx_input = inp })
            (DiscordVty.channelView (pure Nothing))
      unEvent widget 4 `shouldBe` Nothing
      unEvent widget 5 `shouldBe` Just "hello"
      unEvent widget 6 `shouldBe` Nothing

main :: IO ()
main = hspec spec
