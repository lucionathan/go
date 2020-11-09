{-|
    Esse modulo contem a 'main' function
-}
module Main(main) where

import Go

main :: IO()
main = do
    putStrLn $ "Enter 1 to play."
    putStrLn $ "Enter 0 to Exit"
    x <- getLine
    let i = read x
    if i == 1 then playGo else
        if i == 0 then putStrLn ("Thank you") else do
        putStrLn $ "Invalid Input"
        main
