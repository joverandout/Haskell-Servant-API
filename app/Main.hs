module Main where

import Lib

main :: IO ()
main = do
    putStrLn "Running webserver on http://localhost:8080   (CMD Click)"
    startApp
    
