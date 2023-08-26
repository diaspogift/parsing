{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wunused-matches #-}
{-# OPTIONS_GHC -Wmissing-export-lists #-}

module Common where
import Data.Char (digitToInt)
import Data.List (isPrefixOf)


--- ########################################################### ---

--- Building a useful set of parser combinators: Part II

--- ########################################################### ---


--- | Transforming the contents of a parser with “fmap”


parseDigit :: Parser Char
parseDigit = anyOf ['0'..'9']

parseThreeDigits :: Parser ((Char, Char), Char)
parseThreeDigits =
  parseDigit .>>. parseDigit .>>. parseDigit


parseThreeDigits' :: ParseResult (((Char, Char), Char), String)
parseThreeDigits' = run parseThreeDigits "123A"


--- ||| Functor to the rescue


instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p = Parser $ \ str ->
      let rs = run p str
      in case rs of
        Success (x, remainingInput) ->
          Success (f x, remainingInput)
        Failure err -> Failure err

(<!>) :: (a -> b) -> Parser a -> Parser b
( <!> ) = fmap


-- Parsing three digits with fmap

parseThreeDigitsAsStr :: Parser [Char]
parseThreeDigitsAsStr =
  let
    tupleParser =
     parseDigit .>>. parseDigit .>>. parseDigit
    transformTuple ((c1, c2), c3) = [c1, c2, c3 ]
  in fmap transformTuple tupleParser


-- Example 

res1 :: ParseResult ([Char], String)
res1 = run parseThreeDigitsAsStr "123A"


parseThreeDigitsAsInt :: Parser Int
parseThreeDigitsAsInt =
  fmap digitToInt' parseThreeDigitsAsStr
  where
    digitToInt' :: [Char] -> Int
    digitToInt' [] = 0
    digitToInt' ys@(x:xs) =
      digitToInt x * (10 ^ tenPower) +  digitToInt' xs
      where tenPower = length ys - 1



--- | 2. Lifting functions to the world of Parsers (Applicative)


instance Applicative Parser where
    pure :: x -> Parser x
    pure x = Parser $ \ str -> Success (x, str)
    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    pab <*> pa = Parser $ \str ->
      let rpab = run pab str
      in case rpab of
        Success (f, remainingInput) ->
          let rpa = run pa remainingInput
          in case rpa of
            Success (x, nestRemainingInput) ->
              Success (f x, nestRemainingInput)
            Failure err -> Failure err
        Failure err -> Failure err


lift2 :: (a -> b -> c) ->  Parser a -> Parser b -> Parser c
lift2 f xP yP = f <$> xP <*> yP

compareChar :: Char -> Char -> Bool
compareChar c1 c2 = c1 == c2


compareCharLifted2 :: Parser Char -> Parser Char -> Parser Bool
compareCharLifted2 = lift2 compareChar



p1 :: Parser Bool
p1 = compareCharLifted2 (pchar 'A') (pchar 'A')


r1 :: ParseResult (Bool, String)
r1 = run p1 "AA"

r2 :: ParseResult (Bool, String)
r2 = run p1 "ABC"

-- Examples

addIntLifted2 :: Parser Int -> Parser Int -> Parser Int
addIntLifted2 = lift2 (+)

p2 :: Parser Int
p2 = addIntLifted2 undefined undefined

r3 :: ParseResult (Bool, String)
r3 = run p1 "AA"

r4 :: ParseResult (Bool, String)
r4 = run p1 "ABC"



startsWith :: String -> String -> Bool
startsWith = isPrefixOf

startsWithLifted2 :: Parser String -> Parser String -> Parser Bool
startsWithLifted2 = lift2 startsWith



--- | 3. Turning a list of Parsers into a single Parser 
--- | unsing sequence
--- | sequence :: (Traversable t, Monad m) => t (m a) -> m (t a)


instance Monad Parser where
    return :: x -> Parser x
    return = pure
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    pa >>= pab = Parser $ \str ->
      let rpa = run pa str
      in case rpa of
        Success (x, remaining1) ->
          let rpab = pab x
          in run rpab remaining1
        Failure err -> Failure err



-- Implementing the “pstring” parser



pstring :: [Char] -> Parser [Char]
pstring = sequence . fmap pchar


pstring' :: [Char] -> Parser [Char]
pstring' = mapM pchar



--- | 4. Matching a parser multiple times

parseZeroOrMore :: Parser a -> String -> ([a], String)
parseZeroOrMore parser input =
    let firstResult = run parser input
    in case firstResult of
        Failure _ -> ([], input)
        Success (firstValue, inputAfterFirstParse) ->
          let
            (subsequentValues, remainingInput) =
              parseZeroOrMore parser inputAfterFirstParse
            values = firstValue : subsequentValues
          in (values,remainingInput)


many :: Parser a -> Parser [a]
many parser = Parser innerFn
  where
    innerFn str = Success (parseZeroOrMore parser str)


-- Examples

manyA :: Parser [Char]
manyA = many (pchar 'A')

r5 :: ParseResult ([Char], String)
r5 = run manyA "ABCD"

r6 :: ParseResult ([Char], String)
r6 = run manyA "AACD"

r7 :: ParseResult ([Char], String)
r7 = run manyA "AAAD"



r8 :: ParseResult ([Char], String)
r8 = run manyA "|BCD"


--


manyAB :: Parser [[Char]]
manyAB = many (pstring "AB")


r9 :: ParseResult ([[Char]], String)
r9 = run manyAB "ABCD"

r10 :: ParseResult ([[Char]], String)
r10 = run manyAB "ABABCD"

r11 :: ParseResult ([[Char]], String)
r11 = run manyAB "ZCD"

r12 :: ParseResult ([[Char]], String)
r12 = run manyAB "AZCD"


--

whitespaceChar :: Parser Char
whitespaceChar = anyOf [' ', '\t', '\n']

whitespace :: Parser [Char]
whitespace = many whitespaceChar

r13 :: ParseResult ([Char], String)
r13 = run whitespace "ABC"

r14 :: ParseResult ([Char], String)
r14 = run whitespace " ABC"

r15 :: ParseResult ([Char], String)
r15 = run whitespace "\tABC"


-- Defining “many1”

many1 :: Parser a -> Parser [a]
many1 parser = Parser $
 \ str ->
    let firstResult = run parser str
    in case firstResult of
      Failure err -> Failure err
      Success (firstValue, inputAfterFirstParse) ->
        let (subsequentValues,remainingInput) =
             parseZeroOrMore parser inputAfterFirstParse
            values = firstValue :subsequentValues
        in Success (values,remainingInput)

digit :: Parser Char
digit = anyOf ['0'..'9']

digits :: Parser [Char]
digits = many1 digit

r16 :: ParseResult ([Char], String)
r16 = run digits "1ABC"

r17 :: ParseResult ([Char], String)
r17 = run digits "12BC"

r18 :: ParseResult ([Char], String)
r18 = run digits "123C"

r19 :: ParseResult ([Char], String)
r19 = run digits "1234"

r20 :: ParseResult ([Char], String)
r20 = run digits "ABC"


-- Parsing an integer



digitToInt' :: [Char] -> Int
digitToInt' [] = 0
digitToInt' ys@(x:xs) =
   digitToInt x * (10 ^ tenPower) +  digitToInt' xs
   where tenPower = length ys - 1

pint :: Parser Int
pint =
  let
    digit = anyOf ['0'..'9']
    digits = many1 digit
  in fmap digitToInt' digits



r21 :: ParseResult (Int, String)
r21 = run pint "1ABC"

r22 :: ParseResult (Int, String)
r22 = run pint "12BC"

r23 :: ParseResult (Int, String)
r23 = run pint "123C"

r24 :: ParseResult (Int, String)
r24 = run pint "1234"

r25 :: ParseResult (Int, String)
r25 = run pint "ABC"



--- | 5. Matching a parser zero or one time


opt :: Parser a ->  Parser (Maybe a)
opt p =
    let
      j = fmap Just p
      n = return Nothing
    in j `orElse` n

digitThenSemicolon :: Parser (Char, Maybe Char)
digitThenSemicolon = digit .>>. opt (pchar ';')

r26 :: ParseResult ((Char, Maybe Char), String)
r26 = run digitThenSemicolon "1;"

r27 :: ParseResult ((Char, Maybe Char), String)
r27 = run digitThenSemicolon "1"

-- pint with optional minus sign (Functor to the rescue)

pintWithSign :: Parser Int
pintWithSign =
  let
    digit = anyOf ['0'..'9']
    digits = many1 digit
    parser = opt (pchar '-') .>>. digits
  in fmap resultToInt parser
  where
    resultToInt (sign,charList) =
      let i = digitToInt' charList
      in case sign of
        Just _ -> -i
        Nothing  -> i



r28 :: ParseResult (Int, String)
r28 = run pint "123C"

r29 :: ParseResult (Int, String)
r29 = run pint "-123C"


--- | 6. Throwing results away


(.>>) :: Parser a -> Parser b -> Parser a
(.>>) p1 p2 =
    fmap fst $ p1 `andThen` p2


(>>.) :: Parser a -> Parser b -> Parser b
(>>.) p1 p2 =
    fmap snd $ p1 `andThen` p2

-- Improved ditgit then optional semi column 

digitThenSemicolon' :: Parser (Char, Maybe Char)
digitThenSemicolon' = digit .>>. opt (pchar ';')

r33 :: ParseResult ((Char, Maybe Char), String)
r33 = run digitThenSemicolon' "1;"

r34 :: ParseResult ((Char, Maybe Char), String)
r34 = run digitThenSemicolon' "1"


-- White spcaces

whitespaceChar' :: Parser Char
whitespaceChar' = anyOf [' ', '\t', '\n']

whitespace' :: Parser [Char]
whitespace' = many1 whitespaceChar

ab :: Parser [Char]
ab = pstring "AB"

cd :: Parser [Char]
cd = pstring "CD"

abCd :: Parser ([Char], [Char])
abCd = (ab .>> whitespace') .>>. cd

r35 :: ParseResult (([Char], [Char]), String)
r35 = run abCd "AB \t\nCD"


-- Introducing “between”


between :: Parser a -> Parser b -> Parser c -> Parser b
between p1 p2 p3 =
  p1 >>. p2 .>> p3


-- Example


pdoublequote :: Parser Char
pdoublequote = pchar '"'

quotedInteger :: Parser Int
quotedInteger = between pdoublequote pint pdoublequote

r36 :: ParseResult (Int, String)
r36 = run quotedInteger "\"1234\""

r37 :: ParseResult (Int, String)
r37 = run quotedInteger "1234"



--- | 7. Parsing lists with separators





separatedBy1 :: Parser a -> Parser b -> Parser [a]
separatedBy1 p sep = do
  let sepThenP = sep >>. p
  (p,pList) <- p .>>. many sepThenP
  return $ p:pList


separatedBy :: Parser a -> Parser b -> Parser [a]
separatedBy p sep =
    separatedBy1 p sep <|> return []


comma :: Parser Char
comma = pchar ','

digit' :: Parser Char
digit' = anyOf ['0'..'9']

zeroOrMoreDigitList :: Parser [Char]
zeroOrMoreDigitList = digit `separatedBy` comma
oneOrMoreDigitList :: Parser [Char]
oneOrMoreDigitList = digit `separatedBy1`  comma

r38 :: ParseResult ([Char], String)
r38 = run oneOrMoreDigitList "1;"
r39 :: ParseResult ([Char], String)
r39 = run oneOrMoreDigitList "1,2;"
r40 :: ParseResult ([Char], String)
r40 = run oneOrMoreDigitList "1,2,3;"
r41 :: ParseResult ([Char], String)
r41 = run oneOrMoreDigitList "Z;"

r42 :: ParseResult ([Char], String)
r42 = run zeroOrMoreDigitList "1;"
r43 :: ParseResult ([Char], String)
r43 = run zeroOrMoreDigitList "1,2;"
r44 :: ParseResult ([Char], String)
r44 = run zeroOrMoreDigitList "1,2,3;"
r45 :: ParseResult ([Char], String)
r45 = run zeroOrMoreDigitList "Z;"



--- | Reimplementing other combinators using “bind”

andThenWithMonad :: Parser a -> Parser b -> Parser (a, b)
andThenWithMonad p1 p2 = do
  val1 <- p1
  val2 <- p2
  return (val1, val2)

applyWithMonad :: Parser (a -> b) -> Parser a -> Parser b
applyWithMonad pab pa = do
  fab <- pab
  fab <$> pa

many1WithMonad :: Parser a -> Parser [a]
many1WithMonad p = do
  val <- p
  vals <- many p
  return $ val : vals


-- Example




digits'' :: Parser [Char]
digits'' = many1WithMonad digit

r46 :: ParseResult ([Char], String)
r46 = run digits "1ABC"

r47 :: ParseResult ([Char], String)
r47 = run digits "12BC"

r48 :: ParseResult ([Char], String)
r48 = run digits "123C"

r49 :: ParseResult ([Char], String)
r49 = run digits "1234"

r50 :: ParseResult ([Char], String)
r50 = run digits "ABC"






















































































































































































































































































































































































































--- ########################################################### ---

--- Understanding Parser Combinators: Part I

--- ########################################################### ---


--- | Parsing a hard coded character

parseA :: String -> (Bool, String)
parseA str
  | null str = (False, [])
  | head str == 'A' = (True, tail str)
  | otherwise = (False, str)


-- Example

inputABC :: [Char]
inputABC = "ABC"
parseARes :: (Bool, String)
parseARes = parseA inputABC


inputZBC :: [Char]
inputZBC = "ZBC"
parseARes' :: (Bool, String)
parseARes' = parseA inputZBC


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


-- Exemple 

parseAchar :: (String, String)
parseAchar = pchar0 ('A',inputABC)

parseAchar' :: (String, String)
parseAchar' = pchar0 ('A',inputZBC)

parseZchar :: (String, String)
parseZchar = pchar0 ('Z',inputZBC)


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


parseAchar'' :: ParseResult (Char, String)
parseAchar'' = pchar1 ('A',inputABC)


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
pchar3 charToMatch = innerFn
  where
    innerFn str
      | null str = Failure "No more input"
      | charToMatch == head str =
          Success (charToMatch, tail str)
      | otherwise =
          let first = head str
              msg = "Expecting " ++ show charToMatch
                     ++ " but got " ++ show first
          in Failure msg


--- | The benefits of the curried implementation

parseA' :: String -> ParseResult (Char, String)
parseA' = pchar3 'A'

parseAchar''' :: ParseResult (Char, String)
parseAchar''' = parseA' inputABC

parseAchar'''' :: ParseResult (Char, String)
parseAchar'''' = parseA' inputZBC



--- | Encapsulating the parsing function in a type


newtype Parser a = Parser (String -> ParseResult (a, String))

run :: Parser a -> String -> ParseResult (a, String)
run aParser aString =
  let (Parser innerFn) = aParser
  in innerFn aString


pchar :: Char -> Parser Char
pchar charToMatch = Parser innerFn
    where
      innerFn :: String -> ParseResult (Char, String)
      innerFn str
        | null str = Failure "No more input"
        | charToMatch == head str =
             Success (charToMatch, tail str)
        | otherwise =
            let first = head str
                msg = "Expecting " ++ show charToMatch
                       ++ " but got " ++ show first
            in Failure msg


-- Testing the wrapped function

newAParserA :: Parser Char
newAParserA = pchar 'A'

newParseARes :: ParseResult (Char, String)
newParseARes = run newAParserA inputABC

newParseARes' :: ParseResult (Char, String)
newParseARes' = run newAParserA inputZBC


--- | Combining two parsers in sequence



andThen :: Parser a -> Parser b -> Parser (a, b)
andThen parser1 parser2 = Parser innerFn
    where
      -- innerFn :: [Char] -> ParseResult ((a, b), String)
      innerFn str =
        let rs1 = run parser1 str
        in case rs1 of
            Failure err -> Failure err
            Success (first, rest1) ->
              let res2 = run parser2 rest1
              in case res2 of
                Failure err -> Failure err
                Success (second, rest2) ->
                  let founds = (first, second)
                  in Success (founds, rest2)

(.>>.) :: Parser a -> Parser b -> Parser (a, b)
( .>>. ) = andThen


-- Testing “andThen”


parA :: Parser Char
parA = pchar 'A'
parB :: Parser Char
parB = pchar 'B'

parseAThenB :: Parser (Char, Char)
parseAThenB = parA .>>. parB

resParA :: ParseResult ((Char, Char), String)
resParA = run parseAThenB "ABC"

resParB :: ParseResult ((Char, Char), String)
resParB = run parseAThenB "ZBC"

resParC :: ParseResult ((Char, Char), String)
resParC = run parseAThenB "AZC"


--- | Choosing between two parsers


orElse :: Parser a -> Parser a -> Parser a
orElse parser1 parser2 =
    let
      innerFn str =
        let rs1 = run parser1 str
        in case rs1 of
            Success (first, rest1) -> rs1
            Failure err ->
              let rs2 = run parser2 str
              in rs2
    in  Parser innerFn


(<|>) :: Parser a -> Parser a -> Parser a
( <|> ) = orElse


-- Testing “orElse”

paorA :: Parser Char
paorA = pchar 'A'

paorB :: Parser Char
paorB = pchar 'B'

parseAOrElseB :: Parser Char
parseAOrElseB = paorB <|> paorB



res2 :: ParseResult (Char, String)
res2 = run parseAOrElseB "BZZ"

res3 :: ParseResult (Char, String)
res3 = run parseAOrElseB "CZZ"


-- Combining “andThen” and “orElse”

paseA :: Parser Char
paseA = pchar 'A'
paseB :: Parser Char
paseB = pchar 'B'
paseC :: Parser Char
paseC = pchar 'C'


bOrElseC :: Parser Char
bOrElseC = paseB <|> paseC

aAndThenBorC :: Parser (Char, Char)
aAndThenBorC = paseA .>>. bOrElseC


reslt1 :: ParseResult ((Char, Char), String)
reslt1 = run aAndThenBorC "ABZ"

reslt2 :: ParseResult ((Char, Char), String)
reslt2 = run aAndThenBorC "ACZ"

reslt3 :: ParseResult ((Char, Char), String)
reslt3 = run aAndThenBorC "QBZ"

reslt4 :: ParseResult ((Char, Char), String)
reslt4 = run aAndThenBorC "AQZ"


--- | Choosing from a list of parsers


choice :: [Parser a] -> Parser a
choice [] = error "Empty parser list provided"
choice parsers = foldl1 orElse parsers


--- |Using choice to build the anyOf parser


anyOf :: [Char] -> Parser Char
anyOf = choice . fmap pchar


-- Examples

parseLowercase :: Parser Char
parseLowercase = anyOf ['a'..'z']






resParseLowercase :: ParseResult (Char, String)
resParseLowercase = run parseLowercase "aBC"

resParseLowercase' :: ParseResult (Char, String)
resParseLowercase' = run parseLowercase "ABC"

resParseDigit :: ParseResult (Char, String)
resParseDigit = run parseDigit "1ABC"

resParseDigit' :: ParseResult (Char, String)
resParseDigit' = run parseDigit "9ABC"

resParseDigit'' :: ParseResult (Char, String)
resParseDigit'' = run parseDigit "|ABC"




--- References
--- https://fsharpforfunandprofit.com/posts/understanding-parser-combinators/
--- 