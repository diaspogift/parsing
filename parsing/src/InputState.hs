module InputState where


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

fromStr :: String -> InputState
fromStr str
  | null str = InputState {InputState.lines =[], position=initialPos}
  | otherwise = InputState {InputState.lines = words str, position=initialPos}