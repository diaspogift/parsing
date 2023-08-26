{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Lib where

import Control.Applicative
import Data.Char ( digitToInt, isDigit, isSpace )


someFunc :: IO ()
someFunc = putStrLn "someFunc"

--- | Parsing a hard coded character

parseA' :: String -> (Bool, String)
parseA' str
  | null str = (False, [])
  | head str == 'A' = (True, tail str)
  | otherwise = (False, str)


--- | Parsing a specified character


pchar0 :: (Char, String) -> (String, String)
pchar0 (charToMatch,str)
  | null str = ("No more input", [])
  | charToMatch == head str =
      ("Found" ++ show charToMatch, tail str)
  | otherwise =
    let first = head str
        msg = "Expecting " ++ show charToMatch
              ++ " but got " ++ show first
    in (msg, str)


--- | Returning a Success/Failure

data ParseResult a =
    Success a
  | Failure String
  deriving Show


pchar1 :: (Char, String) -> ParseResult (Char, String)
pchar1 (charToMatch,str)
  | null str = Failure "No more input"
  | charToMatch == head str =
      Success (charToMatch, tail str)
  | otherwise =
    let first = head str
        msg = "Expecting " ++ show charToMatch
              ++ " but got " ++ show first
    in Failure msg


--- | Switching to a curried implementation


pchar2 :: Char -> String -> ParseResult (Char, String)
pchar2 charToMatch str
  | null str = Failure "No more input"
  | charToMatch == head str =
      Success (charToMatch, tail str)
  | otherwise =
    let first = head str
        msg = "Expecting " ++ show charToMatch
              ++ " but got " ++ show first
    in Failure msg


--- | Rewriting with an inner function


pchar3 :: Char -> (String -> ParseResult (Char, String))
pchar3 charToMatch =
    let innerFn str
            | null str = Failure "No more input"
            | charToMatch == head str =
                Success (charToMatch, tail str)
            | otherwise =
                let first = head str
                    msg = "Expecting " ++ show charToMatch
                        ++ " but got " ++ show first
                in Failure msg
    in innerFn


--- | Encapsulating the parsing function in a type 


newtype Parser a = Parser {
    runParser :: String -> ParseResult (a, String)
}


pchar :: Char -> Parser Char
pchar charToMatch =
    let innerFn :: String -> ParseResult (Char, String)
        innerFn str
            | null str = Failure "No more input"
            | charToMatch == head str =
                Success (charToMatch, tail str)
            | otherwise =
                let first = head str
                    msg = "Expecting " ++ show charToMatch
                        ++ " but got " ++ show first
                in Failure msg
    in Parser innerFn


pchar' :: Char -> Parser' Char
pchar' charToMatch =
    let label = show charToMatch ++ " parser " :: ParserLabel
        err = "Char parser error" :: ParserLabel
        innerFn :: String -> ParseResult' (Char, String)
        innerFn str
            | null str = Failure' (label, err)
            | charToMatch == head str =
                Success' (charToMatch, tail str)
            | otherwise =
                let first = head str
                    msg = "Unexpected " ++ show first
                in Failure' (label, msg)
    in Parser' {
        run = innerFn
    ,   label = label
    }

--- | Combining two parsers in sequence


instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p = Parser $ \ str ->
        let rs = runParser p str
        in case rs of
            Success (x, str0) -> Success (f x, str0)
            Failure msg -> Failure msg

instance Applicative Parser where
    pure :: x -> Parser x
    pure x = Parser $ \ str -> Success (x, str)
    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    pab <*> pa = Parser $ \str ->
        let rpab = runParser pab str
        in case rpab of
            Success (f, remaininInput) ->
                let rpa = runParser pa remaininInput
                in case rpa of
                    Success (x, str1) -> Success (f x, str1)
                    Failure msg -> Failure msg
            Failure msg0 -> Failure msg0


instance Monad Parser where
    return :: x -> Parser x
    return = pure
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    pa >>= pab = Parser $ \str ->
        let rpa = runParser pa str
        in case rpa of
            Success (x1, remaining1) ->
                let rpab = pab x1
                in runParser rpab remaining1
            Failure err -> Failure err


--- | Product (and) Parser


andThen :: Parser a -> Parser b -> Parser (a, b)
andThen parser1 parser2 =
    let innerFn str =
            let rs1 = runParser parser1 str
            in case rs1 of
                Failure err -> Failure err
                Success (first, rest1) ->
                    let res2 = runParser parser2 rest1
                    in case res2 of
                        Failure err -> Failure err
                        Success (second, rest2) ->
                            let founds = (first, second)
                            in Success (founds, rest2)
    in  Parser innerFn

(.>>.) :: Parser a -> Parser b -> Parser (a, b)
(.>>.) = andThen

--- | Sum (or) Parser 

orElse :: Parser a -> Parser a -> Parser a
orElse parser1 parser2 =
    let innerFn str =
            let rs1 = runParser parser1 str
            in case rs1 of
                Success (first, rest1) -> rs1
                Failure err ->
                    let rs2 = runParser parser2 str
                    in rs2
    in  Parser innerFn

orElse' :: Parser' a -> Parser' a -> Parser' a
orElse' parser1 parser2 =
    let label1 = label parser1
        label2 = label parser2
        innerFn str =
            let rs1 = run parser1 str
            in case rs1 of
                Success' (first, rest1) -> rs1
                Failure' err ->
                    let rs2 = run parser2 str
                    in rs2
    in  Parser' innerFn (label1 ++ " or " ++ label2)

(<|>) :: Parser a -> Parser a -> Parser a
(<|>) = orElse

--- | Combining “andThen” and “orElse”

parseA :: Parser Char
parseA = pchar 'A'
parseB :: Parser Char
parseB = pchar 'B'
parseC :: Parser Char
parseC = pchar 'C'
bOrElseC :: Parser Char
bOrElseC = parseB `orElse` parseC
aAndThenBorC :: Parser (Char, Char)
aAndThenBorC = parseA `andThen` bOrElseC


--- | Choosing from a list of parsers

choice :: [Parser a] -> Parser a
choice [] = error "empty parser list provided"
choice parsers = foldl1 orElse parsers


choice' :: [Parser' a] -> Parser' a
choice' [] = error "empty parser list provided"
choice' parsers = foldl1 orElse' parsers

--- |Using choice to build the anyOf parser


anyOf :: [Char] -> Parser Char
anyOf = choice . fmap pchar

anyOf' :: [Char] -> Parser' Char
anyOf' = choice' . fmap pchar'


parseLowercase :: Parser Char
parseLowercase = anyOf ['a'..'z']

parseDigit :: Parser Char
parseDigit = anyOf ['0'..'9']


--- | Transforming the contents of a parser with “map”

parseThreeDigits :: Parser ((Char, Char), Char)
parseThreeDigits =
    parseDigit
    `andThen`
    parseDigit
    `andThen`
    parseDigit







--- | Implementing the “pstring” parser --- NEEDS TO REVISIT
--- |  WHY IS RHE sequence function not working???

mySequence :: Monad m => [m a] -> m [a]
mySequence parserList =
    let consP = liftA2 (:)
    in case parserList of
        [] -> return []
        (x:xs) -> consP x (mySequence xs)

pstring :: [Char] -> Parser [Char]
pstring = mySequence . fmap pchar


--- | Matching a parser multiple times


parseZeroOrMore :: Parser a -> String -> ([a], String)
parseZeroOrMore parser input =
    let firstResult = runParser parser input
    in case firstResult of
        Failure err -> ([], input)
        Success (firstValue, inputAfterFirstParse) ->
            let (subsequentValues, remainingInput) =
                    parseZeroOrMore parser inputAfterFirstParse
                values = firstValue : subsequentValues
            in  (values,remainingInput)



many :: Parser a -> Parser [a]
many parser =
    let innerFn str =
         Success (parseZeroOrMore parser str)
    in Parser innerFn


--- | Defining “many1”


many1 :: Parser a -> Parser [a]
many1 parser = Parser $
 \ str ->
    let firstResult = runParser parser str
    in case firstResult of
        Failure err -> Failure err
        Success (firstValue, inputAfterFirstParse) ->
            let (subsequentValues,remainingInput) =
                    parseZeroOrMore parser inputAfterFirstParse
                values = firstValue :subsequentValues
            in Success (values,remainingInput)


--- | Parsing an integer

digitToInt' :: [Char] -> Int
digitToInt' [] = 0
digitToInt' ys@(x:xs) =
   digitToInt x * (10 ^ tenPower) +  digitToInt' xs
   where tenPower = length ys - 1

pint :: Parser Int
pint =
    let digit = anyOf ['0'..'9']
        digits = many1 digit
    in fmap digitToInt' digits


--- | Matching a parser zero or one time



opt :: Parser a ->  Parser (Maybe a)
opt p =
    let 
        j = fmap Just p
        n = return Nothing
    in j `orElse` n


digit :: Parser Char
digit = anyOf ['0'..'9']

digitThenSemicolon :: Parser (Char, Maybe Char)
digitThenSemicolon = digit .>>. opt (pchar ';')


--- | EXO TODO And here is pint rewritten to handle an optional minus sign:





--- | Throwing results away


(.>>) :: Parser a -> Parser b -> Parser a
(.>>) p1 p2 =
    fmap fst $ p1 `andThen` p2


(>>.) :: Parser a -> Parser b -> Parser b
(>>.) p1 p2 =
    fmap snd $ p1 `andThen` p2




--- | White space 

whitespaceChar :: Parser Char
whitespaceChar = anyOf [' ', '\t', '\n']

whitespace :: Parser [Char]
whitespace = many1 whitespaceChar


--- | Introducing “between”


between :: Parser a -> Parser b -> Parser c -> Parser b
between p1 p2 p3 =
  p1 >>. p2 .>> p3



--- | Parsing lists with separators


separatedBy1 :: Parser a -> Parser b -> Parser [a]
separatedBy1 p sep = do
  let sepThenP = sep >>. p
  (p,pList) <- p .>>. Lib.many sepThenP
  return $ p:pList



separatedBy :: Parser a -> Parser b -> Parser [a]
separatedBy p sep =
    separatedBy1 p sep Lib.<|> return []



--- | Reimplementing other combinators using “bind”



--- | Listing of the parser library so far








--- | Part III Improving the parser library



--- | 1. Labelling a Parser




--- | Improving the error messages


type ParserLabel = String
type ParserError = String


--- | Defining new data types


data ParseResult' a =
    Success' a
  | Failure' (ParserLabel, ParserError)


data Parser' a = Parser' {
    run :: String -> ParseResult' (a, String)
,   label      :: ParserLabel
}

printResult :: (Show a) => ParseResult' (a, b) -> String
printResult result =
  case result of
    Success' (value, input) -> show value
    Failure' (label,error) ->
        let le = show label ++ " " ++ show error
        in "Error parsing " ++ le



--- | Updating the code



instance Functor Parser' where
    fmap :: (a -> b) -> Parser' a -> Parser' b
    fmap f p = Parser' {
        run = \ str ->
            let rs = run p str
            in case rs of
                Success' (x, str0) -> Success' (f x, str0)
                Failure' msg -> Failure' msg
    ,   label =  let lb = label p in lb

    }



instance Applicative Parser' where
    pure :: forall x. x -> Parser' x
    pure x = Parser' {
        run = \str -> Success' ( x , str)
    ,   label =  mempty
    }
    (<*>) :: Parser' (a -> b) -> Parser' a -> Parser' b
    pab <*> pa = Parser' {
        run = \str ->
            let rpab = run pab str
            in case rpab of
                Success' (f, str0) ->
                    let rpa = run pa str0
                    in case rpa of
                        Success' (x, str1) -> Success' (f x, str1)
                        Failure' msg -> Failure' msg
                Failure' msg0 -> Failure' msg0
    ,   label =  let lb = label pa in lb
    }



instance Monad Parser' where
    return :: x -> Parser' x
    return = pure
    (>>=) :: Parser' a -> (a -> Parser' b) -> Parser' b
    pa >>= pab = Parser' {
        run = \str ->
        let rpa = run pa str
        in case rpa of
            Success' (x1, remaining1) ->
                let rpab = pab x1
                in run rpab remaining1
            Failure' err -> Failure' err
    ,   label =  let lb = label pa in lb
    }



--- | Updating the label


setLabel :: Parser' a -> ParserLabel -> Parser' a
setLabel parser newLabel =
    Parser' {run = newInnerFn, label = newLabel}
    where
        newInnerFn input =
            let result = run parser input
            in case result of
                Success' s -> Success' s
                Failure' (oldLabel, err) ->
                    Failure' (newLabel, err)

(<?>) :: Parser' a -> ParserLabel -> Parser' a
( <?> ) = setLabel


parseDigitWithLabel :: Parser' Char
parseDigitWithLabel =
    anyOf' ['0'..'9'] <?> "digit"



--- | Setting default labels:


andThenWithLabel :: Parser' a -> Parser' b -> Parser' (a, b)
andThenWithLabel parser1 parser2 =
    let innerFn str =
            let rs1 = run parser1 str
            in case rs1 of
                Failure' err -> Failure' err
                Success' (first, rest1) ->
                    let res2 = run parser2 rest1
                    in case res2 of
                        Failure' err -> Failure' err
                        Success' (second, rest2) ->
                            let founds = (first, second)
                            in Success' (founds, rest2)
    in  Parser' innerFn ""


andThenWithLabel' :: Parser' a -> Parser' b -> Parser' (a, b)
andThenWithLabel' parser1 parser2 = do
    let label1 = label parser1
        label2 = label parser2
        labels = show label1 ++ " and then " ++ show label2
    res1 <- parser1
    res2 <- parser2
    return (res1, res2) <?> labels


--orElseWithLabel
--orElseWithLabel

orElseWithLabel :: Parser' a -> Parser' a -> Parser' a
orElseWithLabel parser1 parser2 =
    let label1 = label parser1
        label2 = label parser2
        lbel = (label1 ++ " or else " ++ label2)
        innerFn str =
            let rs1 = run parser1 str
            in case rs1 of
                Success' (first, rest1) -> rs1
                Failure' err ->
                    let rs2 = run parser2 str
                    in rs2
    in  Parser' innerFn lbel <?> lbel



anyOfWithLabel :: [Char] -> Parser' Char
anyOfWithLabel str =
        let lbel = "any of " ++ show str
        in (choice' . fmap pchar') str <?> lbel


--- | Replacing “pchar” with “satisfy”


satisfy :: (Char -> Bool) -> ParserLabel -> Parser' Char
satisfy predicate label =
    let innerFn :: String -> ParseResult' (Char, String)
        innerFn str
            | null str = Failure' (label, "No more input")
            | predicate first  =
                Success' (first, tail str)
            | otherwise =
                let msg = "Unexpected " ++ show first
                in Failure' (label, msg)
            where first = head str
    in Parser' {
        run = innerFn
    ,   label = label
    }


--- | Rewriting pchar with satisfy


pcharWithSatify :: Char -> Parser' Char
pcharWithSatify charToMatch =
  let predicate ch = (ch == charToMatch)
      label = show charToMatch
  in satisfy predicate label


digitCharWithSatisfy :: Parser' Char
digitCharWithSatisfy =
  let predicate = isDigit 
      label = "digit"
  in satisfy predicate label


whitespaceCharWithSatisfy :: Parser' Char
whitespaceCharWithSatisfy =
  let predicate = isSpace 
      label = "white space"
  in satisfy predicate label


--- | 3. Adding position and context to error messages

--- | Defining a input that tracks position


data Position = Position {
   line :: Int
,  column :: Int
}    

initialPos :: Position
initialPos = Position 0 0

incrCol :: Position -> Position
incrCol p = 
    let l = line p
        oldCol = column p
    in Position { line = l,  column = oldCol + 1 }

data InputState = InputState {
  lines :: [String]
, position :: Position
}

--- | We will also need a way to convert a string into a initial InputState:


 
