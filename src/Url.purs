module Url (httpURL, httpsURL, toString, Url, HostPort) where

import Prelude

import Control.Alt ((<|>))
import Data.Array (many, fromFoldable, snoc)
import Data.Array as Array
import Data.Char as Char
import Data.Foldable (fold, intercalate)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int as Int
import Data.List (List, (:))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe', maybe)
import Data.String (toLower)
import Data.String.CodeUnits (fromCharArray, singleton)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafeCrashWith)
import Text.Parsing.StringParser (Parser, fail, try)
import Text.Parsing.StringParser.CodeUnits
  ( alphaNum
  , anyDigit
  , anyLetter
  , char
  , eof
  , oneOf
  , string
  , upperCaseChar
  )
import Text.Parsing.StringParser.Combinators
  ( choice
  , many1
  , optionMaybe
  , sepBy
  )

scheme :: Parser String
scheme = toLower <<< fromCharArray <<< fromFoldable <$> many1 schemeCharacter

schemeCharacter :: Parser Char
schemeCharacter =
  anyLetter
    <|> anyDigit
    <|> upperCaseChar
    <|> oneOf [ '+', '.', '-' ]

type Url = { scheme :: String, hostPort :: HostPort, path :: Array String, search :: Map.Map String (Array String) }
type HostPort = { host :: String, port :: Int }

toString :: Url -> String
toString url =
  fold
    [ url.scheme
    , url.hostPort.host
    , showPort url.hostPort.port
    , showPath url.path
    , showSearch url.search
    ]
  where
  showPort = case _ of
    80 -> ""
    n -> fold [ ":", show n ]
  showPath paths = fold [ "/", intercalate "/" paths ]

  showSearch :: Map.Map String (Array String) -> String
  showSearch s
    | Map.isEmpty s = ""
    | otherwise = fold
        [ "?"
        , intercalate "&"
            <<< join
            <<< fromFoldable
            <<< Map.values
            $ mapWithIndex concatKeyValues s
        ]
  concatKeyValues key values = map (concatKeyValue key) values
  concatKeyValue key value = fold [ key, "=", value ]

httpURL :: Parser Url
httpURL = httpURL' "http://"

httpsURL :: Parser Url
httpsURL = httpURL' "https://"

httpURL' :: String -> Parser Url
httpURL' unparsedScheme = do
  _ <- string unparsedScheme
  parsedHostPort <- hostPort
  parsedPath <- maybe [] fromFoldable <$> optionMaybe (char '/' *> hpath)
  parsedSearch <- fromMaybe Map.empty <$> optionMaybe (char '?' *> search)
  _ <- eof
  pure
    { scheme: unparsedScheme
    , hostPort: parsedHostPort
    , path: parsedPath
    , search: parsedSearch
    }

hostPort :: Parser HostPort
hostPort = do
  parsedHost <- host
  parsedPort <- fromMaybe 80 <$> optionMaybe (char ':' *> port)
  pure { host: parsedHost, port: parsedPort }

host :: Parser String
host = hostNumber <|> hostName

hostName :: Parser String
hostName = do
  domainLabels <- many $ try $ domainLabel <* char '.'
  parsedTopLabel <- topLabel
  pure $ intercalate "." $ snoc domainLabels parsedTopLabel

domainLabel :: Parser String
domainLabel = multiSnakeAlphaNum <|> (singleton <$> alphaNum)
  where
  multiSnakeAlphaNum = do
    first <- singleton <$> alphaNum
    rest <- snakeCased
    pure $ first <> rest

topLabel :: Parser String
topLabel = multiSnakeAnyLetter <|> (singleton <$> anyLetter)
  where
  multiSnakeAnyLetter = do
    first <- singleton <$> anyLetter
    rest <- snakeCased
    pure $ first <> rest

snakeCased :: Parser String
snakeCased = intercalate "-" <$> sepBy (fromCharArray <$> many alphaNum) (char '-')

hostNumber :: Parser String
hostNumber = do
  first <- many1Digits
  _ <- char '.'
  second <- many1Digits
  _ <- char '.'
  third <- many1Digits
  _ <- char '.'
  fourth <- many1Digits
  pure $ intercalate "." [ first, second, third, fourth ]
  where
  many1Digits = fromCharArray <<< fromFoldable <$> many1 anyDigit

port :: Parser Int
port = do
  digits <- many1 anyDigit
  let
    digitsText = fromCharArray <<< fromFoldable $ digits
  maybe (fail "could not parse port as int") pure $ Int.fromString digitsText

hpath :: Parser (List String)
hpath = sepBy hsegment (char '/')

hsegment :: Parser String
hsegment = fromCharArray <$> many (oneOf [ '=', '&', '@', ':', ';' ] <|> uchar)

uchar :: Parser Char
uchar = escape <|> unreserved

escape :: Parser Char
escape = do
  _ <- char '%'
  firstHex <- hex
  secondHex <- hex
  let
    charCode = Int.fromStringAs Int.hexadecimal
      $ fromCharArray [ firstHex, secondHex ]
    theChar = fromMaybe' (\_ -> unsafeCrashWith $ "illegal charcode: " <> show charCode)
      $ Char.fromCharCode =<< charCode
  pure theChar

hex :: Parser Char
hex = anyDigit <|> oneOf
  [ 'A'
  , 'B'
  , 'C'
  , 'D'
  , 'E'
  , 'F'
  , 'a'
  , 'b'
  , 'c'
  , 'd'
  , 'e'
  , 'f'
  ]

unreserved :: Parser Char
unreserved = choice
  [ anyLetter
  , anyDigit
  , safe
  , extra
  ]

safe :: Parser Char
safe = oneOf [ '$', '-', '_', '.', '+' ]

extra :: Parser Char
extra = oneOf [ '!', '*', '\'', '(', ')', ',' ]

search :: Parser (Map.Map String (Array String))
search = toSearch Map.empty <$> sepBy searchKeyValuePair (char '&')
  where
  toSearch result = case _ of
    List.Nil -> result
    (Tuple key value) : xs -> toSearch (Map.alter (upsertValue value) key result) xs
  upsertValue val possibleValues = Just $ case possibleValues of
    Just otherVals -> Array.snoc otherVals val
    Nothing -> [ val ]

searchKeyValuePair :: Parser (Tuple String String)
searchKeyValuePair = do
  key <- keyOrValue
  _ <- char '='
  value <- keyOrValue
  pure $ Tuple key value
  where
  keyOrValue = fromCharArray <$> many (oneOf [ '@', ':', ';' ] <|> uchar)

