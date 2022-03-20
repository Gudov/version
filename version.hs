import Control.Exception
import System.Environment
import Text.Printf
import System.IO
import Data.Time.Clock
import Data.Time.Calendar

readBuildNumber :: FilePath -> IO Int
readBuildNumber fileName = do
  readResult <- Control.Exception.try (evaluate $ readFile' fileName) :: IO (Either SomeException (IO String))
  case readResult of
    Left ex -> return 0
    Right val -> do
      str <- val
      return $ extractBuildNumber $ words $ head $ lines str
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
  buildNumber <- readBuildNumber fileName
  buildDate <- getCurrentTime >>= return . show
  writeFile fileName $ formatBuildString (buildNumber + 1) buildDate