import Control.Monad.Reader

greet :: String -> String
greet name = "Hello, " ++ name ++ ".\n"

farewell :: String -> String
farewell name = "Goodbye, " ++ name ++ ".\n"

smalltalk :: String -> String
smalltalk name = "It's a lovely day today, isn't it " ++ name ++ "?\n"

-- Traditional style
conversation :: String -> String
conversation name = greet name ++ smalltalk name ++ farewell name

-- Applicative style
conversationA :: String -> String
conversationA = go <$> greet <*> smalltalk <*> farewell 
    where go a b c = a ++ b ++ c

-- Sequential
conversationS :: String -> String
conversationS = concat . sequenceA [greet, smalltalk, farewell]

-- Monadic style
conversationM :: String -> String
conversationM = do
    a <- greet
    b <- smalltalk
    c <- farewell
    return (a ++ b ++ c)

-- Using MonadReader
conversationR :: String -> String
conversationR = runReader $ do
    a <- reader greet
    b <- reader smalltalk
    c <- reader farewell
    return (a ++ b ++ c)
    
    


-- original implementation
between10and20 :: Int -> Bool
between10and20 x = x >= 10 && x <= 20

-- monadic style
between10and20' :: Int -> Bool
between10and20' = do
    -- our argument is passed first to (>= 10), the result stored in above10
    above10 <- (>= 10)
    -- our argument is then passed to (<= 20), the result stored in below20
    below20 <- (<= 20)
    -- The final result is true iff above10 and below20 are both true
    return (above10 && below20)
    
-- Applicative style
between10and20'' :: Int -> Bool
-- we apply our argument to (>= 10) and (<= 20), then we apply (&&) to the result.
between10and20'' = (&&) <$> (>= 10) <*> (<= 20)
