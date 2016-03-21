import MyGraph

readInts :: String -> [Int]
readInts = map read . words

parseLine :: String -> (Int, [Int])
parseLine s = (x, xs)
  where (x:xs) = readInts s

main :: IO ()
main = do
  input <- getContents
  let graph = graphFromList $ map parseLine $ lines input in
    putStrLn $ show $ dfs graph
