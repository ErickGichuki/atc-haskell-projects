module TaskManager (addTask, viewTasks, editTask, deleteTask, completeTask) where

import FileHandler (readTasks, writeTasks)

-- Add a new task
addTask :: String -> IO ()
addTask desc = do
  tasks <- readTasks
  writeTasks (tasks ++ [desc ++ " | Incomplete"])
  putStrLn "Task added!"

-- View all tasks
viewTasks :: IO ()
viewTasks = do
  tasks <- readTasks
  if null tasks
    then putStrLn "No tasks found."
    else mapM_ putStrLn $ zipWith (\i t -> show i ++ ". " ++ t) [1 :: Int ..] tasks

-- Edit a task
editTask :: Int -> String -> IO ()
editTask num newDesc = do
  tasks <- readTasks
  if num <= 0 || num > length tasks
    then putStrLn "Invalid task number."
    else do
      let updatedTasks = map (\(i, t) -> if i == num then newDesc ++ " | Incomplete" else t) $ zip [1 :: Int ..] tasks
      writeTasks updatedTasks
      putStrLn "Task updated!"

-- Delete a task
deleteTask :: Int -> IO ()
deleteTask num = do
  tasks <- readTasks
  if num <= 0 || num > length tasks
    then putStrLn "Invalid task number."
    else do
      let updatedTasks = map snd $ filter (\(i, _) -> i /= num) $ zip [1 :: Int ..] tasks
      writeTasks updatedTasks
      putStrLn "Task deleted!"

-- Mark a task as completed
completeTask :: Int -> IO ()
completeTask num = do
  tasks <- readTasks
  if num <= 0 || num > length tasks
    then putStrLn "Invalid task number."
    else do
      let updatedTasks = map (\(i, t) -> if i == num then markComplete t else t) $ zip [1 :: Int ..] tasks
      writeTasks updatedTasks
      putStrLn "Task marked as complete!"

-- Helper to mark a task as completed
markComplete :: String -> String
markComplete task =
  let (desc, _) = break (== '|') task
   in desc ++ " | Complete"
