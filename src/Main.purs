module Main where

import Prelude

import ArrowPlayground.EventMachine (EventSource, merge)
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log "🍝"

counter :: EventSource Unit -> EventSource Unit
counter start = let
  go :: EventSource Int -> EventSource Int
  go feedback = let
    begin = map (const 0) start
    next = delay $  merge begin (map (add 1) feedback)
    in next
