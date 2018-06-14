module Day9 where

removeBangs :: String -> String
removeBangs [] = []
removeBangs ('!' : _ : xs) = removeBangs xs
removeBangs (x : xs) = x : removeBangs xs

removeGarbage :: String -> String
removeGarbage [] = []
removeGarbage ('<' : xs) = removeGarbage $ drop 1 $ dropWhile (/= '>') xs
removeGarbage (x : xs) = x : removeGarbage xs

scoreGroups :: String -> Int
scoreGroups = go 0 0
  where 
    go depth score [] = score
    go depth score ('{' : xs) = go (depth + 1) score xs
    go depth score ('}' : xs) = go (depth - 1) (score + depth) xs
    go depth score (_ : xs) = go depth score xs


process :: String -> Int
process = scoreGroups . removeGarbage . removeBangs

main :: IO ()
main = interact $ show . process
