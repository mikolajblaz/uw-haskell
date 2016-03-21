import System.Environment
import System.IO.Error
import MyGraph

readInts :: String -> [Int]
readInts = map read . words

parseLine :: String -> (Int, [Int])
parseLine s = (x, xs)
  where (x:xs) = readInts s

main :: IO ()
main = do
  args <- getArgs

  eInput <- let readInput = case args of {
      [] -> getContents;
      (arg:_) -> readFile arg
    } in tryIOError(readInput)

  case eInput of
      Left e -> if isDoesNotExistError e
        then print "Error: file does not exist"
        else print "Error: file could not be read"

      Right inputStr -> if null parsedFile
          then print "Error: file is empty"
          else print $ dfs $ graphFromList parsedFile
        where parsedFile = map parseLine $ filter (not . null) $ lines inputStr
