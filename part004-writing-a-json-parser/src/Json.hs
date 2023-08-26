module Json where

import CoreWithPosition (pstring, Parser (..), ParseResult (Success, Failure), (<|>), (<?>), satisfy, pchar, ParserLabel, orElse, choice, printResult)
import InputState (fromStr, InputState)
import Data.Map ( Map )



--- |  1. Building a model to represent the JSON spec


data JValue =
    JNull
  | JString String
  | JNumber Float
  | JBool Bool
  | JObject  (Map String JValue)
  | JArray  [JValue]
  deriving Show



--- | 2. Getting started with “Null” and “Bool”



--- Parsing Null



jNull :: Parser JValue
jNull =
  let
    res = pstring "null"
    f = const JNull
    lbl =  "null"
  in fmap f res <?> lbl


r001 :: ParseResult (JValue, InputState)
r001 = run jNull $ fromStr "null"


r002 :: ParseResult (JValue, InputState)
r002 = run jNull $ fromStr "nulp"



--- Parsing Bool



jBool :: Parser [Char]
jBool =
  let
    jtrue = pstring "true"
    jfalse = pstring "false"
  in jtrue <|> jfalse <?> "bool"


r003 :: ParseResult ([Char], InputState)
r003 = run jBool $ fromStr "true"

r004 :: ParseResult ([Char], InputState)
r004 = run jBool $ fromStr "false"

r005 :: ParseResult ([Char], InputState)
r005 = run jBool $ fromStr "truX"



--- | 3. Parsing “String”


jUnescapedChar :: Parser Char
jUnescapedChar =
  let label = "char"
  in satisfy (\ch -> ch /= '\\' && ch /= '\"') label

r006 :: ParseResult (Char, InputState)
r006 = run jUnescapedChar $ fromStr "a"

r007 :: ParseResult (Char, InputState)
r007 = run jUnescapedChar $ fromStr "\\"





jEscapedChar :: Parser Char
jEscapedChar =
  let
    escapes = [("\\\"",'\"'), ("\\\\",'\\'), ("\\/",'/'), ("\\b",'\b'), ("\\f",'\f'), ("\\n",'\n'), ("\\r",'\r'), ("\\t",'\t')]
    plist = fmap f escapes
  in choice plist <?> "escaped char"
  where
    f (toMatch, result) =
      result <$ pstring toMatch





r008 :: ParseResult (Char, InputState)
r008 = run jEscapedChar $ fromStr "\\\\"

r009 :: ParseResult (Char, InputState)
r009 = run jEscapedChar $ fromStr "\\t"

r010 :: ParseResult (Char, InputState)
r010 = run jEscapedChar $ fromStr "\\"

r011 :: ParseResult (Char, InputState)
r011 = run jEscapedChar $ fromStr "\n"

r012 :: ParseResult (Char, InputState)
r012 = run jEscapedChar $ fromStr "a"