module Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console as Console
import Text.Parsing.StringParser (runParser)
import Url as Url

main :: Effect Unit
main = do
  Console.log "httpURL parser"
  Console.log $ show $ runParser Url.httpURL "http://www.google.com"


