module InputState where

data Position = Position {
   line :: Int
,  column :: Int
}

data InputState = InputState {
  lines :: [String]
, position :: Position
}


initialPos :: Position
initialPos = Position {
  line=0
, column=0
}

fromStr :: String -> InputState
fromStr str
  | null str = InputState {InputState.lines = [], position = initialPos}
  | otherwise = InputState {InputState.lines = words str, position = initialPos}



