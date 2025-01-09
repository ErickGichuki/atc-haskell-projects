module UserInteraction (displayHelp, handleInput) where

import TaskManager (addTask, viewTasks, editTask, deleteTask, completeTask)

-- Display help menu
displayHelp :: IO ()
displayHelp = do
  putStrLn "TODO List Manager Commands:"
  putStrLn "  add      - Add a new task"
  putStrLn "  view     - View all tasks"
  putStrLn "  edit     - Edit a task"
  putStrLn "  delete   - Delete a task"
  putStrLn "  complete - Mark a task as complete"
  putStrLn "  exit     - Exit the program"
  putStrLn "  help     - Show this help menu"

-- Handle user input
handleInput :: String -> IO Bool
handleInput "exit" = do
  putStrLn "Goodbye!"
  pure False
handleInput "add" = do
  putStr "Enter task description: "
  desc <- getLine
  addTask desc
  pure True
handleInput "view" = do
  viewTasks
  pure True
handleInput "edit" = do
  putStr "Enter task number to edit: "
  num <- readLn
  putStr "Enter new description: "
  desc <- getLine
  editTask num desc
  pure True
handleInput "delete" = do
  putStr "Enter task number to delete: "
  num <- readLn
  deleteTask num
  pure True
handleInput "complete" = do
  putStr "Enter task number to mark as complete: "
  num <- readLn
  completeTask num
  pure True
handleInput "help" = displayHelp >> pure True
handleInput _ = putStrLn "Unknown command." >> pure True
