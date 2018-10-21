import System.IO

bottles :: Int -> String
bottles 0 = "better go to the store and buy some more." 
bottles x = show x       ++ " bottles of beer on the wall\n"                    ++
            show x       ++ " bottles of beer\nTake one down, pass it around\n" ++ 
            show (x - 1) ++ " bottles of beer on the wall\n\n"                  ++ 
            bottles (x - 1)

main = putStrLn $ bottles 99