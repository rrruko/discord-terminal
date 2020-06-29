{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec

import qualified DiscordVty (scrollableText')

import Control.Monad.Fix
import Control.Monad.Reader
import Reflex
import Reflex.Pure
import Reflex.Vty
import qualified Graphics.Vty as V
import qualified Data.Text as T

runVtyWidgetPure
  :: (t ~ Pure Int)
  => VtyWidgetCtx t
  -> VtyWidget t ((->) Int) a
  -> Int -> (a, Behavior t [V.Image])
runVtyWidgetPure ctx w = runReaderT (runBehaviorWriterT (unVtyWidget w)) ctx

-- Hack to allow pure evaluation of reflex-vty widgets.
-- getNextNodeId is rarely actually invoked but the constraint occurs
-- on many functions in reflex-vty. It should be possible to define
-- a pure StateT-based implementation instead of just failing like this.
instance MonadNodeId ((->) a) where
  getNextNodeId = error "MonadNodeId ((->) a): Unimplemented"

spec :: SpecWith ()
spec = do

  describe "scrollableText'" $ do
    it "stays within bounds" $ do
      let
        inp = Event $ \t -> Just (V.EvKey V.KDown [])
        widgetContents = pure (T.lines "A\nB\nC\nD\nE")
        vtyCtx windowHeight =
          VtyWidgetCtx
            (constDyn 80)
            (constDyn windowHeight)
            (constDyn True)
            inp
        widget windowHeight =
          runVtyWidgetPure
            (vtyCtx windowHeight)
            (DiscordVty.scrollableText' never widgetContents)
      unBehavior (fst $ widget 3 0) 0 `shouldBe` (1,3,5)
      unBehavior (fst $ widget 3 0) 1 `shouldBe` (2,4,5)
      unBehavior (fst $ widget 3 0) 2 `shouldBe` (3,5,5)
      unBehavior (fst $ widget 3 0) 100 `shouldBe` (3,5,5)
      unBehavior (fst $ widget 100 0) 0 `shouldBe` (1,5,5)

main :: IO ()
main = hspec spec
