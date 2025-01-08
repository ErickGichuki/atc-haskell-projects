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