filterMaybe :: (a -> Bool) -> [Maybe a] -> [Maybe a]
filterMaybe p xs = 
    let 
        transform (Just x) = if p x then Just x else Nothing
        transform Nothing = Nothing
    -- in map transform xs
    in filter isJust (map transform xs)
  where
    isJust (Just _) = True
    isJust Nothing = False


main :: IO ()
main = do
    putStrLn "(>2) к числам"
    let test1 = filterMaybe (>2) [Just 3, Just 1, Nothing, Just 4]
    print test1
