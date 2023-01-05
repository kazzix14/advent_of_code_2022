import Data.Char (isLower, isUpper, ord)
import Data.List (intersect)
import Data.List.Split

main :: IO ()
main = do
  ls <- lines <$> readFile "day3.txt"
  let byGroups = chunksOf 3 ls
  let dupPriorities = itemToPriority . findDup <$> byGroups
  print $ sum dupPriorities

findDup :: [String] -> Char
findDup (s : ss) = head $ foldr intersect s ss
findDup _ = error "unexpected"

itemToPriority :: Char -> Int
itemToPriority c
  | isLower c = ord c - 96
  | isUpper c = ord c - 64 + 26
  | otherwise = error "unexpected"
