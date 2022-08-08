module Driver where

import Algebra
import Model
import Interpreter

-- Exercise 11
interactive :: Environment -> ArrowState -> IO ()
interactive = undefined

-- Depending on the result of the previous step it decides what it will do next
-- If it is done it prints the end state
-- If Ok then it will perform the next command
-- If fail it returns the current state it failed in
batch :: Environment -> ArrowState -> (Space, Pos, Heading)
batch env arr@(ArrowState space pos@(y,x) hdir [])                  = case stepResult of
                                                                        (Done k l m) -> (k, l, m)
                                                                        (Ok state) -> batch env state
                                                                        (Fail _) -> (space, pos, hdir)
    where stepResult = step env arr
batch env arr@(ArrowState space pos@(y,x) hdir stack@(cmd:cmds))    = case stepResult of
                                                                        (Done k l m) -> (k, l, m)
                                                                        (Ok state) -> batch env state
                                                                        (Fail _) -> (space, pos, hdir)
    where stepResult = step env arr

runProgram :: Environment -> ArrowState -> IO ()
runProgram = undefined 