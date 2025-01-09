module FileHandler (ensureTasksFile, readTasks, writeTasks) where

import System.Directory (doesFileExist)
import System.IO (writeFile)

-- Ensure the tasks file exists
ensureTasksFile :: FilePath -> IO ()
ensureTasksFile filePath = do
  exists <- doesFileExist filePath
  if not exists
    then writeFile filePath ""
    else return ()

-- Read tasks from the file
readTasks :: IO [String]
readTasks = lines <$> readFile "tasks.txt"

-- Write tasks to the file
writeTasks :: [String] -> IO ()
writeTasks tasks = writeFile "tasks.txt" (unlines tasks)
