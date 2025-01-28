module TaskManager where

import System.Directory (doesFileExist)
import System.IO (writeFile, appendFile, readFile)
import Data.List (isPrefixOf)
import System.IO (hFlush, stdout)

-- Task Data Types

data Status = Complete
            | Active
            | Upcoming
            deriving (Show, Eq, Read)

data Priority = High
           | Medium
           | Low
           deriving (Show, Eq, Read, Ord)

-- Task type definition
data Task = Task
  { description :: String
  , status      :: Status
  , priority    :: Priority
  } deriving (Show, Eq)

-- Function to create a new task
createTask :: String -> String -> String -> Task
createTask desc statusStr priorityStr = Task
  { description = desc
  , status = read statusStr
  , priority = read priorityStr
  }

-- Function to convert Task to a string for file storage
taskToLine :: Task -> String
taskToLine (Task desc status priority) =
  desc ++ " | " ++ show status ++ " | " ++ show priority

-- Function to read a task from a line in the file
readTask :: String -> Task
readTask line =
  let [desc, statusStr, priorityStr] = wordsBy (== '|') line
  in createTask desc statusStr priorityStr

-- Ensure the tasks file exists
ensureTasksFile :: IO ()
ensureTasksFile = do
  exists <- doesFileExist "tasks.txt"
  if not exists
    then writeFile "tasks.txt" ""
    else return ()

-- Add a new task
addTask :: String -> String -> String -> IO ()
addTask desc status priority = do
  let task = createTask desc status priority
  appendFile "tasks.txt" (taskToLine task ++ "\n")
  putStrLn "Task added!"

-- View all tasks
viewTasks :: IO ()
viewTasks = do
  contents <- readFile "tasks.txt"
  let tasks = lines contents
  if null tasks
    then putStrLn "No tasks found."
    else mapM_ putStrLn tasks

-- Filter tasks by completion status
viewFilteredTasks :: String -> IO ()
viewFilteredTasks statusEnum = do
  contents <- readFile "tasks.txt"
  let tasks = map readTask (lines contents)
      filteredTasks = filter (\t -> show (status t) == statusEnum) tasks
  if null filteredTasks
    then putStrLn $ "No " ++ statusEnum ++ " tasks found."
    else mapM_ (putStrLn . taskToLine) filteredTasks

-- Edit a task
editTask :: Int -> String -> String -> String -> IO ()
editTask num newDesc newStatus newPriority = do
  contents <- readFile "tasks.txt"
  let tasks = map readTask (lines contents)
  if num <= 0 || num > length tasks
    then putStrLn "Invalid task number."
    else do
      let taskToEdit = tasks !! (num - 1)
      if status taskToEdit == Complete
        then putStrLn "This task is already complete and cannot be edited."
        else do
          let updatedTasks = unlines $ map taskToLine $
                         map (\(i, t) -> if i == num
                                          then createTask newDesc newStatus newPriority
                                          else t) 
                         (zip [1..] tasks)
          writeFile "tasks.txt" (init updatedTasks)
          putStrLn "Task updated!"

-- Delete a task with confirmation
deleteTask :: Int -> IO ()
deleteTask num = do
  contents <- readFile "tasks.txt"
  let tasks = map readTask (lines contents)
  if num <= 0 || num > length tasks
    then putStrLn "Invalid task number."
    else do
      -- Display the task to be deleted for confirmation
      let taskToDelete = tasks !! (num - 1)
      putStrLn $ "Are you sure you want to delete this task?"
      putStrLn $ taskToLine taskToDelete
      putStr "Enter 'y' to confirm, 'n' to cancel: "
      hFlush stdout
      confirmation <- getLine
      if confirmation == "y"
        then do
          let updatedTasks = unlines $ map taskToLine $
                             map snd $ filter (\(i, _) -> i /= num) (zip [1..] tasks)
          writeFile "tasks.txt" updatedTasks
          putStrLn "Task deleted!"
        else
          putStrLn "Task deletion canceled."


-- Mark a task as completed
completeTask :: Int -> IO ()
completeTask num = do
  contents <- readFile "tasks.txt"
  let tasks = map readTask (lines contents)
  if num <= 0 || num > length tasks
    then putStrLn "Invalid task number."
    else do
      let updatedTasks = unlines $ map taskToLine $
                         map (\(i, t) -> if i == num
                                          then t { status = Complete }
                                          else t)
                         (zip [1..] tasks)
      writeFile "tasks.txt" (init updatedTasks)
      putStrLn "Task marked as complete!"

-- Helper function to split string by a delimiter
wordsBy :: (Char -> Bool) -> String -> [String]
wordsBy p s =  case dropWhile p s of
                 "" -> []
                 s' -> w : wordsBy p s''
                       where (w, s'') = break p s'

