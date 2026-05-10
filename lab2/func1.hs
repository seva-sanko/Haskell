mu :: (a -> c) -> (b -> c) -> Either a b -> c
mu f _ (Left a) = f a
mu _ g (Right b) = g b

ma :: (a -> b -> c) -> [Maybe a] -> [Maybe b] -> [Maybe c]
ma f (x : xs) (y : ys) = case (x, y) of 
    (Just a, Just b) -> Just(f a b) : ma f xs ys
    _  -> Nothing : ma f xs ys

mi :: (a -> a -> a) -> [Either a b] -> a -> a
mi f (x:xs) acc = case x of 
 Right _ -> mi f xs acc
 Left l -> mi f xs (f l acc)

mo :: (b -> b) -> [Either a b] -> ([Either a b], [Either a b])
mo f (x:xs) = case x of
    Left a  -> (rights, Left a : lefts)
    Right b -> (Right (f b) : rights, lefts)
  where
    (rights, lefts) = mo f xs