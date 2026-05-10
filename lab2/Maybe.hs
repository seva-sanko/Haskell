processMaybe :: (a -> b) -> [Maybe a] -> [Maybe b]
processMaybe f xs = 
    let
        transform (Just x) = Just (f x)
        transform Nothing = Nothing
    in map transform xs


main :: IO ()
main = do
    putStrLn "(+1) к числам"
    let test1 = processMaybe (+1) [Just 2, Just 3, Nothing]
    print test1
