import Data.Maybe
data Shape = Rock | Paper | Scissors deriving (Show, Eq);

instance Ord Shape where
  compare Rock Paper = LT
  compare Paper Scissors = LT
  compare Scissors Rock = LT
  compare l r = compare EQ (compare r l)
  
readShape :: Char -> Maybe Shape 
readShape 'A' = Just Rock
readShape 'B' = Just Paper
readShape 'C' = Just Scissors
readShape 'X' = Just Rock
readShape 'Y' = Just Paper
readShape 'Z' = Just Scissors
readShape _ = Nothing

readsShapes :: [Char] -> [Shape]
readsShapes = mapMaybe readShape

shapeScore :: Shape -> Int
shapeScore Rock = 1
shapeScore Paper = 2
shapeScore Scissors = 3
  
matchScore :: Shape -> Shape -> Int
matchScore mine his
  | his < mine = 6
  | mine < his = 0
  | otherwise = 3

score :: String -> Int
score i = do
  let s = readsShapes i
  let his = head s
  let mine = last s
  shapeScore mine + matchScore mine his

main :: IO ()
main = do
  ls <- lines <$> readFile "day2.txt"
  let scores = score <$> ls
  print $ sum scores


    