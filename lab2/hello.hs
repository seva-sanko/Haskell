processEither :: (a -> a) -> [Either a b] -> ([Either a b], [Either a b])
processEither f xs = 
    let 
        transform (Left x) = Left (f x)
        transform (Right x) = Right x
        
        transformed = map transform xs
        
        rights = [x | x@(Right _) <- transformed]  -- x :: Either a b
        lefts = [x | x@(Left _) <- transformed]    -- x :: Either a b
        
    in (rights, lefts)

main :: IO ()
main = do
    putStrLn "Тест 1: (+4) к числам"
    let test1 = processEither (+4) [Right (5 :: Int), Left (2 :: Int), Right 1, Left 3]
    print test1
    -- Ожидаем: ([Right 5, Right 1], [Left 6, Left 7])
    
    putStrLn "\nТест 2: Только Right значения"
    let test2 = processEither (*2) [Right "hello", Right "world"]
    print test2
    -- Ожидаем: ([Right "hello", Right "world"], [])
    