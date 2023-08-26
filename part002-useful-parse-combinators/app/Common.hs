module Common where

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

res1 :: ParseResult (Char, String)
res1 = run parseAOrElseB "AZZ"

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

parseDigit :: Parser Char
parseDigit = anyOf ['0'..'9']




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