{-
  (C) 2017 David Lettier
  lettier.com
-}

module Main where

import Prelude

import Control.Monad.Eff (Eff)

import UI (runApp, Effects)

main :: Eff (Effects ()) Unit
main = runApp
