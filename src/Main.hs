module Main where

import System.IO (hFlush, stdout)
import FileHandler (ensureTasksFile)
import UserInteraction (displayHelp, handleInput)

main :: IO ()
main = do
  putStrLn "Welcome to my TODO List Manager!"
  ensureTasksFile "tasks.txt"
  displayHelp
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
