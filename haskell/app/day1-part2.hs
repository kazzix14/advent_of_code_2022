import Text.Read ( readMaybe )
import Data.List ( groupBy, sort )
import Data.Maybe ( isJust, catMaybes )

main :: IO ()
main = do
  s <- readFile "input.txt"
  let parsed = parseStr s
  let calories = map sum parsed
  let sorted = reverse $ sort calories
  let top3 = take 3 sorted
  let m = sum top3
  print m

parseStr :: String -> [[Int]]
parseStr s = do
  let splitInput = lines s
  let parsed = map parseLine splitInput
  let grouped = groupBy shouldBeGrouped parsed
  let unwrapped = map catMaybes grouped
  filter (not . null) unwrapped


parseLine :: String -> Maybe Int
parseLine = readMaybe

shouldBeGrouped :: Maybe Int -> Maybe Int -> Bool
shouldBeGrouped a b = isJust a && isJust b

