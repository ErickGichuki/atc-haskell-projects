module TaskManager where

import System.Directory (doesFileExist)
import System.IO (writeFile, appendFile, readFile)

-- Ensure the tasks file exists
ensureTasksFile :: IO ()
ensureTasksFile = do
  exists <- doesFileExist "tasks.txt"
  if not exists
    then writeFile "tasks.txt" ""
    else return ()

-- Add a new task
addTask :: String -> IO ()
addTask desc = appendFile "tasks.txt" (desc ++ " | Incomplete\n") >> putStrLn "Task added!"

-- View all tasks
viewTasks :: IO ()
viewTasks = do
  contents <- readFile "tasks.txt"
  let tasks = lines contents
  if null tasks
    then putStrLn "No tasks found."
    else mapM_ putStrLn $ zipWith (\i t -> show i ++ ". " ++ t) [1 :: Int ..] tasks

-- Edit a task
editTask :: Int -> String -> IO ()
editTask num newDesc = do
  contents <- readFile "tasks.txt"
  let tasks = lines contents
  if num <= 0 || num > length tasks
    then putStrLn "Invalid task number."
    else do
      let updatedTasks = unlines $ map (\(i, t) -> if i == num then newDesc ++ " | Incomplete" else t) $ zip [1 :: Int ..] tasks
      writeFile "tasks.txt" updatedTasks
      putStrLn "Task updated!"

-- Delete a task
deleteTask :: Int -> IO ()
deleteTask num = do
  contents <- readFile "tasks.txt"
  let tasks = lines contents
  if num <= 0 || num > length tasks
    then putStrLn "Invalid task number."
    else do
      let updatedTasks = unlines $ map snd $ filter (\(i, _) -> i /= num) $ zip [1 :: Int ..] tasks
      writeFile "tasks.txt" updatedTasks
      putStrLn "Task deleted!"

-- Mark a task as completed
completeTask :: Int -> IO ()
completeTask num = do
  contents <- readFile "tasks.txt"
  let tasks = lines contents
  if num <= 0 || num > length tasks
    then putStrLn "Invalid task number."
    else do
      let updatedTasks = unlines $ map (\(i, t) -> if i == num then markComplete t else t) $ zip [1 :: Int ..] tasks
      writeFile "tasks.txt" updatedTasks
      putStrLn "Task marked as complete!"

-- Helper to mark a task as completed
markComplete :: String -> String
markComplete task =
  let (desc, _) = break (== '|') task
   in desc ++ " | Complete"
