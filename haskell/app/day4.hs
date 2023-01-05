import Data.Char (isDigit)

data Range = Range Int Int

main :: IO ()
main = do
  ls <- lines <$> readFile "day4.txt"
  let pairs = uncurry includesAnother . parseLine <$> ls
  print $ length $ filter (== True) pairs

parseLine :: String -> (Range, Range)
parseLine i = do
  let (ll, llRest) = takeDigits i
  let (lr, lrRest) = takeDigits (drop 1 llRest)
  let (rl, rlRest) = takeDigits (drop 1 lrRest)
  let (rr, _) = takeDigits (drop 1 rlRest)
  (Range ll lr, Range rl rr)

takeDigits :: String -> (Int, String)
takeDigits i = do
  let ds = takeWhile isDigit i
  let rest = drop (length ds) i
  (read ds, rest)

includesAnother :: Range -> Range -> Bool
includesAnother (Range ll lr) (Range rl rr)
  | ll <= rl && rr <= lr = True
  | rl <= ll && lr <= rr = True
  | otherwise = False
