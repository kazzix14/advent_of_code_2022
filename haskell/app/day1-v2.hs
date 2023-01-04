import Text.Read ( readMaybe )
import Data.List ( groupBy )
import Data.Maybe ( isJust, catMaybes )

main :: IO ()
main = do
  s <- readFile "day1.txt"
  let ls = lines s
  let maybes = readMaybe <$> ls :: [Maybe Int]
  let calories = map (sum . catMaybes) $ groupBy isJusts maybes
  print $ maximum calories
  where
    isJusts a b = isJust a && isJust b