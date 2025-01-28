module Main where

import System.IO (hFlush, stdout)
import qualified TaskManager as TM

main :: IO ()
main = do
  putStrLn "Welcome to my TODO List Manager!"
  TM.ensureTasksFile
  loop

loop :: IO ()
loop = do
  putStr "Enter command (add/view/filter/edit/delete/complete/exit/help): "
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
  putStr "Enter status (Active/Upcoming/Complete): "
  hFlush stdout
  status <- getLine
  putStr "Enter priority (High/Medium/Low): "
  hFlush stdout
  priority <- getLine
  TM.addTask desc status priority
  pure True
handleInput "view" = do
  TM.viewTasks
  pure True
handleInput "filter" = do
  putStr "Filter by status (Active/Upcoming/Complete): "
  hFlush stdout
  status <- getLine
  TM.viewFilteredTasks status
  pure True
handleInput "edit" = do
  putStr "Enter task number to edit: "
  hFlush stdout
  num <- getLine
  putStr "Enter new description: "
  hFlush stdout
  desc <- getLine
  putStr "Enter new status (Active/Upcoming/Complete): "
  hFlush stdout
  status <- getLine
  putStr "Enter new priority (High/Medium/Low): "
  hFlush stdout
  priority <- getLine
  TM.editTask (read num) desc status priority
  pure True
handleInput "delete" = do
  putStr "Enter task number to delete: "
  hFlush stdout
  num <- getLine
  TM.deleteTask (read num)
  pure True
handleInput "complete" = do
  putStr "Enter task number to mark as complete: "
  hFlush stdout
  num <- getLine
  TM.completeTask (read num)
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
handleInput "-h" = handleInput "help"
handleInput "--help" = handleInput "help"
handleInput input = do
  putStrLn $ "Unknown command: " ++ input
  pure True
