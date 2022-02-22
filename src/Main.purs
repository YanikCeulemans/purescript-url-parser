module Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console as Console
import Url as Url

main :: Effect Unit
main = do
  Console.log "httpURL parser"
  Console.log $ show $ Url.parse "http://www.google.com"


