--import Control.Concurrent (threadDelay)
--
--main :: IO ()
--main = do
--    putStrLn "Hello there!"
--    threadDelay 1000000
--    putStrLn "What is your name?"
--    name <- getLine
--    putStrLn $ "Hello, " ++ name ++ "."

getNumber :: IO Int
getNumber = do
    string <- getLine
    pure (read string)

main :: IO ()
main = do
    putStrLn "Please enter a number"
    num1 <- getNumber
    putStrLn "Please enter another number"
    num2 <- getNumber
    let num3 = num1 + num2
    putStrLn (show num1 ++ " + " ++ show num2 ++ " = " ++ show num3)
