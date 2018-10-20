{--
   For compile this program you nedd System.Random
   Install it with the command "stack install random"
   Then run "stack ghc paper-stone-scissor.hs"
--}

import System.IO
import System.Random

number_to_move :: Int -> String
number_to_move m =
    if m == 0
        then "paper"
    else if m == 1
        then "stone"
    else "scissor"


move_to_number :: String -> Int
move_to_number m =
    if m == "paper"
        then 0
    else if m == "stone"
        then 1
    else if m == "scissor"
        then 2
    else 3

find_winner :: Int -> Int -> String
find_winner c u = 
    if (u == 0 && c == 1) || (u == 1 && c == 2) || (u == 2 && c == 0)
        then "User Win!"
    else if u == c
        then "Drew!"
    else if u == 3
        then "User choise is invalid!"
    else "Computer Win!"

main = do
    computer_move <- randomRIO (0, 2) :: IO Int
    putStrLn "Your move:"
    user_move <- getLine
    putStrLn "Computer move:"
    putStrLn $ number_to_move computer_move
    putStrLn $ find_winner computer_move $ move_to_number user_move
    main
