import Control.Exception
import System.Environment
import Text.Printf
import System.IO
import Data.Time.Clock
import Data.Time.Calendar
import System.Directory

maybeReadFile :: String -> IO (Maybe String)
maybeReadFile fileName = do
  fileExist <- doesFileExist fileName
  case fileExist of
    True -> do 
      content <- readFile' fileName
      return $ Just $ content
    False -> return Nothing

parseVersion str = 
    extractBuildNumber $ words $ head $ lines str
  where
    extractBuildNumber [_, _, number] = read number :: Int
    extractBuildNumber _ = 0 :: Int


formatBuildString :: Int -> String -> String
formatBuildString buildNumber buildDate = printf (
  "#define BUILD_NUMBER %d\n" ++
  "#define BUILD_NUMBER_STR \"%d\"\n" ++
  "#define BUILD_DATE \"%s\"\n"
  ) buildNumber buildNumber buildDate

main = do
  args <- getArgs
  let fileName = head args
  fileContent <- maybeReadFile fileName
  let buildNumber = case fileContent of
                      Just content -> parseVersion content
                      Nothing -> 0
  buildDate <- getCurrentTime >>= return . show
  writeFile fileName $ formatBuildString (buildNumber + 1) buildDate