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

spec :: SpecWith ()
spec = do
  describe "scrollableText'" $ do
    it "stays within bounds" $ do
      let
        inp = Event $ \t -> Just (V.EvKey V.KDown [])
        widgetContents = pure "A\nB\nC\nD\nE"
        vtyCtx =
          VtyWidgetCtx
            (constDyn 80)
            (constDyn 3)
            (constDyn True)
            inp
        widget =
          runVtyWidgetPure
            vtyCtx
            (DiscordVty.scrollableText' never widgetContents)
      unBehavior (fst $ widget 0) 0 `shouldBe` (1,3,5)
      unBehavior (fst $ widget 0) 1 `shouldBe` (2,4,5)
      unBehavior (fst $ widget 0) 2 `shouldBe` (3,5,5)
      unBehavior (fst $ widget 0) 100 `shouldBe` (3,5,5)

main :: IO ()
main = hspec spec
