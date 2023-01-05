import Data.Char (isDigit)

data Range = Range Int Int

main :: IO ()
main = do
  ls <- lines <$> readFile "day4.txt"
  let pairs = uncurry overlapsAnother . parseLine <$> ls
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

overlapsAnother :: Range -> Range -> Bool
overlapsAnother (Range ll lr) (Range rl rr)
  | ll <= rl && rl <= lr = True
  | ll <= rr && rr <= lr = True
  | rl <= ll && ll <= rr = True
  | rl <= lr && lr <= rr = True
  | otherwise = False
