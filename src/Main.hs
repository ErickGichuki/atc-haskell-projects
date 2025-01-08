module Main where

import System.IO (hFlush, stdout)
import System.Directory (doesFileExist, removeFile)

main :: IO ()
main = do
  putStrLn "Welcome to my TODO List Manager!"
  ensureTasksFile
  loop

loop :: IO ()
loop = do
  putStr "Enter command (add/view/edit/delete/complete/exit/help): "
  hFlush stdout
  input <- getLine
  isLooping <- handleInput input
  if isLooping
    then loop
    else return ()

handleInput :: String -> IO Bool
handleInput "exit" = do
  putStrLn "Goodbye!"
  pure False
handleInput "add" = do
  putStr "Enter task description: "
  hFlush stdout
  desc <- getLine
  addTask desc
  pure True
handleInput "view" = do
  viewTasks
  pure True
handleInput "edit" = do
  putStr "Enter task number to edit: "
  hFlush stdout 
  num <- getLine
  putStr "Enter new description: "
  hFlush stdout
  desc <- getLine
  editTask (read num) desc
  pure True
handleInput "complete" = do
  putStr "Enter task number to mark as complete: "
  hFlush stdout
  num <- getLine
  completeTask (read num)
  pure True
handleInput "help" = do
  putStrLn "TODO List Manager Commands:"
  putStrLn "  add      - Add a new task"
  putStrLn "  view     - View all tasks"
  putStrLn "  edit     - Edit a task"
  putStrLn "  delete   - Delete a task"
  putStrLn "  complete - Mark a task as complete"
  putStrLn "  exit     - Exit the program"
  putStrLn "  help     - Show this help menu"
  pure True
handleInput "-h" = handleInput "help"  -- Treat -h as --help
handleInput "--help" = handleInput "help"  -- Handle --help
handleInput input = do
  putStrLn $ "Unknown command: " ++ input
  pure True

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