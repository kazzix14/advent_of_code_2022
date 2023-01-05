import Data.Char (isLower, isUpper, ord)
import Data.List (intersect)

main :: IO ()
main = do
  ls <- lines <$> readFile "day3.txt"
  let dupPriorities = itemToPriority . findDup <$> ls
  print $ sum dupPriorities

findDup :: String -> Char
findDup l = do
  let (component0, component1) = splitAt (div (length l) 2) l
  head $ intersect component0 component1

itemToPriority :: Char -> Int
itemToPriority c
  | isLower c = ord c - 96
  | isUpper c = ord c - 64 + 26
  | otherwise = error "unexpected"
