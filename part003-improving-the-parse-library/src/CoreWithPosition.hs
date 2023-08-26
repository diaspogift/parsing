{-# LANGUAGE InstanceSigs #-}
module CoreWithPosition where
import Data.Char (isDigit, isSpace, digitToInt)
import qualified InputState as I
import InputState ( Position(..), InputState(..) )


--- ########################################################### ---

--- Improving the parser library: Part III

--- ########################################################### ---



--- | 1. Labelling a Parser, Improving the error 

type ParserLabel = String
type ParserError = String



printResult :: Show a => ParseResult (a, b) -> String
printResult result =
  case result of
    Success (value, input) -> show value
    Failure (label, error, parserPos) ->
      let
        errorLine = pcurrentLine parserPos
        colPos = pcolumn parserPos
        linePos = pline parserPos
        failureCaret = show colPos ++ " " ++ show error
      in
        "Line: " ++
        show linePos ++
        " Col: " ++
        show colPos ++
        " Error parsing " ++
        show label ++
        "\n " ++
        show errorLine ++
        "\n " ++
        show failureCaret



--- | Updating the code



instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p = Parser {
      run = \ str ->
        let rs = run p str
        in case rs of
          Success (x, remainingInput) ->
            Success (f x, remainingInput)
          Failure err -> Failure err
    , label =  let lb = label p in lb

    }

(<!>) :: (a -> b) -> Parser a -> Parser b
( <!> ) = fmap



instance Applicative Parser where
    pure :: x -> Parser x
    pure x = Parser {
      run = \str -> Success ( x , str)
    , label =  mempty
    }
    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    pab <*> pa = Parser {
      run = \str ->
        let rpab = run pab str
        in case rpab of
          Success (f, remainingInput) ->
            let rpa = run pa remainingInput
            in case rpa of
              Success (x, nestRemainingInput) ->
                Success (f x, nestRemainingInput)
              Failure err -> Failure err
          Failure err -> Failure err
    , label =  let lb = label pa in lb
    }



instance Monad Parser where
    return :: x -> Parser x
    return = pure
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    pa >>= pab = Parser {
      run = \str ->
        let rpa = run pa str
        in case rpa of
          Success (x, remainingInput) ->
            let rpab = pab x
            in run rpab remainingInput
          Failure err -> Failure err
    , label = "unknown" -- let lb = label pa in lb
    }

--- | Updating the label


setLabel :: Parser a -> ParserLabel -> Parser a
setLabel parser newLabel =
  Parser {run = newInnerFn, label = newLabel}
  where
    newInnerFn input =
      let result = run parser input
      in case result of
        Success s -> Success s
        Failure (oldLabel, err, pp) ->
          Failure (newLabel, err, pp)

(<?>) :: Parser a -> ParserLabel -> Parser a
( <?> ) = setLabel




-- Testing 


parseDigitWithLabel :: Parser Char
parseDigitWithLabel =
  anyOf ['0'..'9'] <?> "digit"

r1 :: ParseResult (Char, InputState)
r1 = run parseDigitWithLabel $ I.fromStr "|ABC"


--- | Setting default labels:


andThen :: Parser a -> Parser b -> Parser (a, b)
andThen parser1 parser2 = do
  let
    label1 = label parser1
    label2 = label parser2
    labels = show label1 ++ " and then " ++ show label2
  res1 <- parser1
  res2 <- parser2
  return (res1, res2) <?> labels

(.>>.) :: Parser a -> Parser b -> Parser (a, b)
(.>>.) = andThen

orElse :: Parser a -> Parser a -> Parser a
orElse parser1 parser2 =
  let
    label1 = label parser1
    label2 = label parser2
    lbel = (label1 ++ " or else " ++ label2)
    innerFn str =
      let rs1 = run parser1 str
      in case rs1 of
        Success (first, rest1) -> rs1
        Failure err ->
          let rs2 = run parser2 str
          in rs2
  in  Parser innerFn lbel <?> lbel


(<|>) :: Parser a -> Parser a -> Parser a
( <|> ) = orElse


choice :: [Parser a] -> Parser a
choice [] = error "Empty parser list provided"
choice parsers =
  let
    lbl = "Choice of ..." :: ParserLabel
  in foldl1 orElse parsers <?> lbl


anyOf :: [Char] -> Parser Char
anyOf input =
  let lb = "any of " ++ show input
      pa = (choice . fmap pchar) input
  in  pa <?> lb



--- | 2. Replacing “pchar” with “satisfy”



satisfy :: (Char -> Bool) -> ParserLabel -> Parser Char
satisfy predicate label =
  let
    innerFn input =
      let (remainingInput, charOpt) = nextChar input
      in case charOpt of
        Nothing ->
          let
            err = "No more input"
            pos = parserPositionFromInputState input
          in  Failure (label, err, pos)
        Just first ->
          if predicate first
          then Success (first, remainingInput)
          else
            let
              err = "Unexpected " ++ show first
              pos = parserPositionFromInputState input
            in Failure (label, err, pos)
  in Parser {
        run = innerFn
    ,   label = label
    }



pchar :: Char -> Parser Char
pchar charToMatch =
  let predicate ch = ch == charToMatch
      label = show charToMatch
  in satisfy predicate label


digitChar :: Parser Char
digitChar =
  let
    predicate = isDigit
    label = "digit" :: ParserLabel
  in satisfy predicate label


whitespaceChar :: Parser Char
whitespaceChar =
  let predicate = isSpace
      label = "white space"
  in satisfy predicate label


--- | 3. Adding position and context error messages


-- Defining an input that tracks position



initialPos :: Position
initialPos = Position 0 0

incrCol :: Position -> Position
incrCol p =
    let l = line p
        oldCol = column p
    in
      Position {
        line = l
      , column = oldCol + 1
      }


incrLine :: Position -> Position
incrLine p =
  let
    c = column p
    oldLine = line p
  in
    Position {
     line = oldLine + 1
    ,column = 0
  }


currentLine :: InputState -> String
currentLine inputState =
  let
    linePos = line $ position inputState
  in
    if linePos < nbrOfLines
    then I.lines inputState !! linePos
    else "end of file"
    where
      nbrOfLines =
        length $ I.lines inputState



nextChar :: InputState -> (InputState, Maybe Char)
nextChar input =
  let
    linePos = line $ position input
    colPos = column $ position input
  in
    if linePos >= length (I.lines input)
    then (input, Nothing)
    else
      let
        currentLines = currentLine input
      in
        if colPos < length currentLines
        then
          let
            char = currentLines !! colPos
            newPos = incrCol $ position input

            newState = InputState {
              I.lines = I.lines input
            , position = newPos
            }
          in (newState, Just char)
      else
        let
          char = '\n'
          newPos = incrLine $ position input
          newState = InputState {
               I.lines = I.lines input
            , position = newPos
            }
        in (newState, Just char)


readAllChars :: InputState -> [Char]
readAllChars input =
  let (remainingInput, charOpt) = nextChar input
  in case charOpt of
    Nothing -> []
    Just ch ->
      let rest = readAllChars remainingInput
      in ch : rest

-- Examples

r2 :: [Char]
r2 = readAllChars $ I.fromStr ""

r3 :: [Char]
r3 = readAllChars $ I.fromStr "a"

r4 :: [Char]
r4 = readAllChars $ I.fromStr "ab"

r5 :: [Char]
r5 = readAllChars $ I.fromStr "a\nb"


--- | Changing the parser to use the new input type

data ParserPosition = ParserPosition {
  pcurrentLine :: String
, pline :: Int
, pcolumn :: Int
}

parserPositionFromInputState :: InputState -> ParserPosition
parserPositionFromInputState inputState = ParserPosition {
  pcurrentLine = currentLine inputState
, pline = line $ position inputState
, pcolumn = column $ position inputState
}


data ParseResult a =
    Success a
  | Failure  (ParserLabel, ParserError, ParserPosition)

data Parser a = Parser {
  run :: InputState -> ParseResult (a, InputState)
, label :: ParserLabel
}

exampleError :: ParseResult a
exampleError =
  Failure
  ("identifier", "unexpected |"
  , ParserPosition {pcurrentLine = "123 ab|cd"
  , pline = 1, pcolumn=6}
  )


--- | Fixing up the run function 
--- already taken care of by the definition of the Parser type



---



--- | Fixing up the combinators
--- function satisfy already taken care of




--- | Testing the positional errors


parseAB :: Parser (Char, Char)
parseAB =
  pchar 'A' .>>. pchar 'B' <?> "AB"



--- | 4. Adding some standard parsers to the library
-- pchar, anyOf already done

parseZeroOrMore :: Parser a -> InputState -> ([a], InputState)
parseZeroOrMore parser input =
    let firstResult = run parser input
    in case firstResult of
        Failure err -> ([], input)
        Success (firstValue, inputAfterFirstParse) ->
            let (subsequentValues, remainingInput) =
                    parseZeroOrMore parser inputAfterFirstParse
                values = firstValue : subsequentValues
            in  (values,remainingInput)

many :: Parser a -> Parser [a]
many parser =
    let innerFn input =
         Success (parseZeroOrMore parser input)
    in Parser innerFn "many ..."


many1 :: Parser a -> Parser [a]
many1 parser = Parser innerFn lbl
  where
    innerFn input =
      let firstResult = run parser input
      in case firstResult of
        Failure err -> Failure err
        Success (firstValue, inputAfterFirstParse) ->
          let (subsequentValues,remainingInput) =
                parseZeroOrMore parser inputAfterFirstParse
              values = firstValue :subsequentValues
          in Success (values,remainingInput)
    lbl = "many 1 ..."


manyChars :: Parser a -> Parser [a]
manyChars = many

manyChars1 :: Parser a -> Parser [a]
manyChars1 = many1


pstring :: [Char] -> Parser [Char]
pstring str =
  let lbl = str
  in mapM pchar str <?> lbl


r6 :: ParseResult ([Char], InputState)
r6 = run (pstring "AB") $ I.fromStr "ABC"

r7 :: ParseResult ([Char], InputState)
r7 = run (pstring "AB") $ I.fromStr "A|C"




--- | Whitespace parsers
--- Already taken care of whitespaceChar 




spaces :: Parser [Char]
spaces = many whitespaceChar

spaces1 :: Parser [Char]
spaces1 = many1 whitespaceChar


r8 :: ParseResult ([Char], InputState)
r8 = run spaces  $ I.fromStr " ABC"

r9 :: ParseResult ([Char], InputState)
r9 = run spaces  $ I.fromStr "A"

r10 :: ParseResult ([Char], InputState)
r10 = run spaces1  $ I.fromStr " ABC"

r11 :: ParseResult ([Char], InputState)
r11 =  run spaces1  $ I.fromStr "A"



--- | Numeric parsers
--- digitChar already taken care of

digitsToInt :: [Char] -> Int
digitsToInt [] = 0
digitsToInt ys@(x:xs) =
   digitToInt x * (10 ^ tenPower) +  digitsToInt xs
   where tenPower = length ys - 1

digitsToFloat :: String -> String -> Float
digitsToFloat ipart dpart =
  let
    i = digitsToInt ipart
    d = digitsToInt dpart
  in fromIntegral i  + fromIntegral d / 10^length dpart


opt :: Parser a ->  Parser (Maybe a)
opt p =
  let
    j = fmap Just p
    n = return Nothing
  in j `orElse` n



pint :: Parser Int
pint = res <?> label
  where
    label = "integer"
    resultToInt (sign, digits) =
      let i = digitsToInt digits -- ignore int overflow for now
      in case sign of
        Just ch -> -i  -- negate the int
        Nothing -> i
    digits = manyChars1 digitChar
    res = fmap resultToInt $ opt (pchar '-') .>>. digits

pfloat :: Parser Float
pfloat = res <?> label
  where
    label = "float"
    resultToFloat (((sign, digits1), point), digits2) =
      let fl = digitsToFloat digits1 digits2 -- ignore int overflow for now
      in case sign of
        Just ch -> -fl -- negate the int
        Nothing -> fl
    digits = manyChars1 digitChar
    pRes = opt (pchar '-') .>>. digits .>>. pchar '.' .>>. digits
    res = fmap resultToFloat pRes


r12 :: ParseResult (Int, InputState)
r12 = run pint $ I.fromStr "-123Z"

r13 :: ParseResult (Int, InputState)
r13 = run pint $ I.fromStr "-Z123"

r14 :: ParseResult (Float, InputState)
r14 = run pfloat $ I.fromStr "-123.45Z"

r15 :: ParseResult (Float, InputState)
r15 = run pfloat $ I.fromStr "-123Z45"



--- | 5. Backtracking
--- | To be done / discussed as a exercise

