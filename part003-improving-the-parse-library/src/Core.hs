{-# LANGUAGE InstanceSigs #-}
module Core where
import Data.Char (isDigit, isSpace)
import qualified InputState as I
import InputState ( Position(..), InputState(..) )


--- ########################################################### ---

--- Improving the parser library: Part III

--- ########################################################### ---



--- | 1. Labelling a Parser, Improving the error 

type ParserLabel = String
type ParserError = String


data ParseResult a =
    Success a
  | Failure (ParserLabel, ParserError)


data Parser a = Parser {
    run :: String -> ParseResult (a, String)
,   label :: ParserLabel
}

printResult :: (Show a) => ParseResult (a, b) -> String
printResult result =
  case result of
    Success (value, input) -> show value
    Failure (label,error) ->
      let le = show label ++ " " ++ show error
      in "Error parsing " ++ le


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
    , label =  let lb = label pa in lb
    }


setLabel :: Parser a -> ParserLabel -> Parser a
setLabel parser newLabel =
  Parser {run = newInnerFn, label = newLabel}
  where
    newInnerFn input =
      let result = run parser input
      in case result of
        Success s -> Success s
        Failure (oldLabel, err) ->
          Failure (newLabel, err)

(<?>) :: Parser a -> ParserLabel -> Parser a
( <?> ) = setLabel




-- Testing 


parseDigitWithLabel :: Parser Char
parseDigitWithLabel =
  anyOf ['0'..'9'] <?> "digit"

r1 :: ParseResult (Char, String)
r1 = run parseDigitWithLabel "|ABC"


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







































































pchar :: Char -> Parser Char
pchar charToMatch =
  let
    label = show charToMatch ++ " parser " :: ParserLabel
    err = "Char parser error" :: ParserLabel
    innerFn :: String -> ParseResult (Char, String)
    innerFn str
      | null str = Failure (label, err)
      | charToMatch == head str =
          Success (charToMatch, tail str)
      | otherwise =
          let first = head str
              msg = "Unexpected " ++ show first
          in Failure (label, msg)
    in Parser {
        run = innerFn
    ,   label = label
    }


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
choice parsers = foldl1 orElse parsers


anyOf :: [Char] -> Parser Char
anyOf input =
  let lb = "any of " ++ show input
      pa = (choice . fmap pchar) input
  in  pa <?> lb



--- | 2. Replacing “pchar” with “satisfy”



satisfy :: (Char -> Bool) -> ParserLabel -> Parser Char
satisfy predicate label =
  let 
    innerFn :: String -> ParseResult (Char, String)
    innerFn str
      | null str = Failure (label, "No more input")
      | predicate first  =
          Success (first, tail str)
      | otherwise =
          let msg = "Unexpected " ++ show first
          in Failure (label, msg)
        where first = head str
  in Parser {
        run = innerFn
    ,   label = label
    }



pchar' :: Char -> Parser Char
pchar' charToMatch =
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

{-
data ParseResult a =
    Success a
  | Failure  (ParserLabel, ParserError, ParserPosition)

-}
