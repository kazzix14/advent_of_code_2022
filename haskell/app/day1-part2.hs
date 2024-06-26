import Text.Read ( readMaybe )
import Data.List ( groupBy, sort )
import Data.Maybe ( isJust, catMaybes )

main :: IO ()
main = do
  s <- readFile "day1.txt"
  let ls = lines s
  let maybes = readMaybe <$> ls :: [Maybe Int]
  let calories = map (sum . catMaybes) $ groupBy isJusts maybes
  let top3 = take 3 $ reverse $ sort calories
  print $ sum top3
  where
    isJusts a b = isJust a && isJust b