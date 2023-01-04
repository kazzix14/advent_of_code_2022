import Text.Read ( readMaybe )
import Data.List ( groupBy )
import Data.Maybe ( isJust, catMaybes )

main :: IO ()
main = do
  s <- readFile "input.txt"
  let parsed = parseStr s
  let summed = map sum parsed
  let m =  maximum summed
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

