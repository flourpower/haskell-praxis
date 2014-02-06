main = do
  contents <- getContents
  let answer = process contents
  putStrLn answer

process :: String -> String
process = take 10 . show . sum . map read . lines