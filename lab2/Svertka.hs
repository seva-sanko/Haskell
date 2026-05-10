foldFilter :: (a -> a -> a) -> (a -> Bool) -> [a] -> a -> a
foldFilter op pred xs init = 
    foldl op init (filter pred xs)
    -- foldr op init (filter pred xs)
{-
foldFilter _ _ [] acc = acc
foldFilter op pred (x:xs) acc
    | pred x = foldFilter op pred xs (op acc x)
    | otherwise = foldFilter op pred xs acc
-}

main :: IO ()
main = do
    putStrLn "Тест 1: (+) (>2) [1,2,3,1,5] 0"
    let test1 = foldFilter (+) (>2) [1,2,3,1,5] 0
    print test1
