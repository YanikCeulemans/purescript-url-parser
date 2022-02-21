module Test.Main where

import Prelude

import Data.Either (isLeft)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Url (HostPort, Url, httpURL, httpsURL, toString)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldContain, shouldEqual, shouldSatisfy)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Text.Parsing.StringParser (runParser)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "urlToString" do
    testStringify
      { scheme: "http://"
      , hostPort:
          { host: "www.google.com"
          , port: 80
          }
      , path: [ "search" ]
      , search: Map.fromFoldable [ Tuple "query" [ "test" ], Tuple "limit" [ "50" ] ]
      }
      "http://www.google.com/search?limit=50&query=test"
    testStringify
      { scheme: "https://"
      , hostPort:
          { host: "www.google.com"
          , port: 8080
          }
      , path: []
      , search: Map.empty
      }
      "https://www.google.com:8080/"
  describe "httpURL parser" do
    testParseUrlHost "http://www.google.com" "www.google.com"
    testParseUrlPort "http://www.google.com" { host: "www.google.com", port: 80 }
    testParseUrlHost "http://www.google.co.uk" "www.google.co.uk"
    testParseUrlHost "http://ww2.google.co.uk" "ww2.google.co.uk"
    testParseUrlHost "http://ww2.go--ogle.co.uk" "ww2.go--ogle.co.uk"
    testParseUrlHost "http://w-w-2.go--ogle.c-o.uk" "w-w-2.go--ogle.c-o.uk"
    testParseUrlHost "http://www.google.com2" "www.google.com2"
    testParseUrlHost "http://www.google.com/" "www.google.com"
    testParseUrlHost "http://www.google.com/search" "www.google.com"
    testParseUrlSearch "http://www.google.com/?query=1&query=2"
      $ Map.fromFoldable [ Tuple "query" [ "1", "2" ] ]
    testParseUrlData "http://www.google.com/?s=test&limit=50"
      { scheme: "http://"
      , hostPort:
          { host: "www.google.com"
          , port: 80
          }
      , path: [ "" ]
      , search: Map.fromFoldable [ Tuple "s" [ "test" ], Tuple "limit" [ "50" ] ]
      }
    testParseUrlData "http://www.google.com/search?s=test&limit=50"
      { scheme: "http://"
      , hostPort:
          { host: "www.google.com"
          , port: 80
          }
      , path: [ "search" ]
      , search: Map.fromFoldable [ Tuple "s" [ "test" ], Tuple "limit" [ "50" ] ]
      }
    testParseUrlPath "http://www.google.com//" [ "", "" ]
    testParseUrlPath "http://www.google.com/a;A-_.+$!*'(),019=/%40%20%5a"
      [ "a;A-_.+$!*'(),019=", "@ Z" ]
    testParseUrlPort "http://www.google.com:88" { host: "www.google.com", port: 88 }
    testParseUrlPort "http://192.168.0.1:8080" { host: "192.168.0.1", port: 8080 }
    testParseUrlError "http://www.google.com/%0"
    testParseUrlError "http://www.google.com/%zz"
    testParseUrlError "http://www.google.com:"
    testParseUrlError "http://www.google.com:9.9"
    testParseUrlError "http://www.google.2com"
    testParseUrlError "http://www.google-.2com"
    testParseUrlError "http://www.go*gle.com"
  describe "httpsURL parser" do
    testParseSecureUrlHost "https://www.google.com" "www.google.com"
    testParseSecureUrlHost "https://alm.myfocus.app/jira/browse/TOOL-1381" "alm.myfocus.app"

testStringify :: Url -> String -> Spec Unit
testStringify url expected =
  it ("should stringify to " <> expected) do
    toString url `shouldEqual` expected

testParseSecureUrlHost :: String -> String -> Spec Unit
testParseSecureUrlHost unparsedUrl expected =
  it ("should parse " <> unparsedUrl) do
    let
      hostPortToTest = _.host <<< _.hostPort <$> runParser httpsURL unparsedUrl
    hostPortToTest `shouldContain` expected

testParseUrlPort :: String -> HostPort -> Spec Unit
testParseUrlPort unparsedUrl expected =
  it ("should parse " <> unparsedUrl) do
    let
      hostPortToTest = _.hostPort <$> runParser httpURL unparsedUrl
    hostPortToTest `shouldContain` expected

testParseUrlHost :: String -> String -> Spec Unit
testParseUrlHost unparsedUrl expected =
  it ("should parse " <> unparsedUrl) do
    let
      hostPortToTest = _.host <<< _.hostPort <$> runParser httpURL unparsedUrl
    hostPortToTest `shouldContain` expected

testParseUrlSearch :: String -> Map.Map String (Array String) -> Spec Unit
testParseUrlSearch unparsedUrl expected =
  it ("should parse " <> unparsedUrl <> " having the correct search") do
    let
      searchToTest = _.search <$> runParser httpURL unparsedUrl
    searchToTest `shouldContain` expected

testParseUrlError :: String -> Spec Unit
testParseUrlError unparsedUrl =
  it ("should not parse " <> unparsedUrl) do
    runParser httpURL unparsedUrl `shouldSatisfy` isLeft

testParseUrlPath :: String -> Array String -> Spec Unit
testParseUrlPath unparsedUrl expectedPath =
  it ("should parse " <> unparsedUrl <> " having path " <> show expectedPath) do
    let
      actual = _.path <$> runParser httpURL unparsedUrl
    actual `shouldContain` expectedPath

testParseUrlData :: String -> Url -> Spec Unit
testParseUrlData unparsedUrl expected =
  it ("should parse " <> unparsedUrl <> " into correct Url data") do
    runParser httpURL unparsedUrl `shouldContain` expected


