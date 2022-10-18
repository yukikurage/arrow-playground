module Main where

import Prelude

import ArrowPlayground.EventMachine (EventSource)
import Data.Profunctor.Cochoice (unleft)
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log "🍝"
