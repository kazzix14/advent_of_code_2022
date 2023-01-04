import Data.Foldable (find)
data Shape = Rock | Paper | Scissors deriving (Show, Eq);

instance Ord Shape where
  compare Rock Paper = LT
  compare Rock Scissors = GT
  compare Paper Scissors = LT
  compare Paper Rock = GT
  compare Scissors Rock = LT
  compare Scissors Paper = GT
  compare _ _ = EQ

shapes :: [Shape]
shapes = [Rock, Paper, Scissors]

data Result = Lose | Draw | Win deriving (Enum, Show, Eq);

scoreShape :: Shape -> Int
scoreShape Rock = 1
scoreShape Paper = 2
scoreShape Scissors = 3

scoreResult :: Result -> Int
scoreResult Lose = 0
scoreResult Draw = 3
scoreResult Win = 6

myShape :: Shape -> Result -> Shape
myShape his r = do
  let cmp = case r of
              Lose -> (< his)
              Draw -> (his ==)
              Win -> (his <)

  case find cmp shapes of
    Just s -> s
    Nothing -> error "unreachable"
  
readShape :: Char -> Shape 
readShape 'A' = Rock
readShape 'B' = Paper
readShape 'C' = Scissors
readShape _ = error "unexpected"

readResult :: Char -> Result
readResult 'X' = Lose
readResult 'Y' = Draw
readResult 'Z' = Win
readResult _ = error "unexpected"

readLine :: [Char] -> (Shape, Result)
readLine i = do
  let his = readShape $ head i  
  let r = readResult $ i !! 2
  (his, r)

score :: Shape -> Result -> Int
score his r = do
  let m = myShape his r
  scoreShape m + scoreResult r

main :: IO ()
main = do
  ls <- lines <$> readFile "day2.txt"
  let scores = uncurry score . readLine <$> ls
  print $ sum scores


    