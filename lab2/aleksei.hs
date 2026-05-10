import Distribution.Simple.Utils (xargs)
import System.Posix (accessModes)
-- Еще примеры:
increment :: Int -> Int
increment x = x + 1

negateBool :: Bool -> Bool
negateBool x = not x

double :: Double -> Double
double x = x * 2



add :: Int -> Int -> Int
add x y = x + y

-- Конкатенация для списков
concatLists :: [a] -> [a] -> [a]
concatLists x y = x ++ y

-- Логическое И
andOp :: Bool -> Bool -> Bool
andOp x y = x && y

-- Произвольная функция a -> a -> a
combine :: a -> a -> a
combine x y = x  -- всегда возвращает первый аргумент

-- Выбор первого элемента
const :: a -> b -> a
const x y = x

multiply :: Int -> Int -> Int
multiply x y = x * y




-- Получить первый элемент списка
head :: [a] -> a
head (x:xs) = x

-- Получить хвост списка
tail :: [a] -> [a]
tail (x:xs) = xs

-- Проверить пуст ли список
null' :: [a] -> Bool
null' [] = True
null' _  = False

-- Длина списка
length' :: Num a => [a] -> a
length' [] = 0
length' (x:xs) = 1 + length' xs

sumList :: Num a => [a] -> a
sumList = foldl (+) 0

sumList2 :: Num a => [a] -> a
sumList2 [] = 0
sumList2 (x:xs) = foldl (+) x xs


-- Рекурсивное сложение
sumList3 :: [Int] -> Int
sumList3 [] = 0
sumList3 (x:xs) = x + sumList xs

-- Рекурсивная конкатенация
concatLists2 :: [[a]] -> [a]
concatLists2 [] = []
concatLists2 (x:xs) = x ++ concatLists2 xs

-- Рекурсивное преобразование
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

productList :: Num a => [a] -> a
productList [] = 1
productList (x:xs) = x * productList xs




-- Извлечение значения из Maybe с значением по умолчанию
fromMaybe :: a -> Maybe a -> a
fromMaybe defaultVal Nothing = defaultVal
fromMaybe _ (Just x) = x

-- Применение функции к значению в Maybe
mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe _ Nothing = Nothing
mapMaybe f (Just x) = Just (f x)

-- Цепочка операций с Maybe
andThen :: Maybe a -> (a -> Maybe b) -> Maybe b
andThen Nothing _ = Nothing
andThen (Just x) f = f x

fff :: [Maybe a] -> [Maybe a]
fff [] = []
fff (x:xs) = case x of
    Just x -> (Just x) : fff xs
    Nothing -> fff xs


f :: [Maybe a] -> [Maybe a] -> [(Maybe a, Maybe a)]
f [] _ = []
f _ [] = []
f (x:xs) (y:ys) = (x,y) : f xs ys

p :: [a] -> [(Int, a)]
p [] = []

-- Если оба значения Just, верни их сумму, иначе Nothing
addMaybes :: Maybe Int -> Maybe Int -> Maybe Int
addMaybes _ Nothing = Nothing
addMaybes Nothing _ = Nothing
addMaybes (Just x) (Just y) = Just (x + y)

-- Верни первый Just из двух значений, или Nothing
firstJust :: Maybe a -> Maybe a -> Maybe a
firstJust _ Nothing = Nothing
firstJust Nothing _ = Nothing
firstJust (Just x) (Just y) = (Just x)


-- Безопасное получение головы списка
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

-- Безопасное получение хвоста
safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:xs) = Just xs

-- Фильтрация Maybe значений
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing:xs) = catMaybes xs
catMaybes (Just x:xs) = x : catMaybes xs


sumJust :: [Maybe Int] -> Int
sumJust xs = sum [x | Just x <- xs]


-- Напиши функцию, которая делит два числа, но возвращает Nothing при делении на ноль
safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (div x y)


-- Напиши функцию, которая складывает два Maybe Int
addMaybe :: Maybe Int -> Maybe Int -> Maybe Int
addMaybe (Just x) (Just y) = Just (x + y)
addMaybe _ _ = Nothing


-- Примени Maybe функцию к Maybe значению
applyMaybe :: Maybe (a -> b) -> Maybe a -> Maybe b
applyMaybe (Just f) (Just x) = Just (f x)
applyMaybe _ _ = Nothing


-- Отфильтруй только Just значения из списка
catMaybes' :: [Maybe a] -> [a]
catMaybes' [] = []
catMaybes' (Just x:xs) = x : catMaybes' xs
catMaybes' (Nothing:xs) = catMaybes' xs

-- Если все элементы списка Just, верни Just списка значений
-- Иначе верни Nothing
allJust :: [Maybe a] -> Maybe [a]
allJust [] = Just []
allJust (Just x:xs) = case allJust xs of
    Just rest -> Just (x : rest)
    Nothing -> Nothing
allJust (Nothing:_) = Nothing

-- Вычисли: (x + y) * z, если все значения Just
calcSequence :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int
calcSequence mx my mz =
    case mx of
        Just x -> case my of
            Just y -> case mz of
                Just z -> Just ((x + y) * z)
                Nothing -> Nothing
            Nothing -> Nothing
        Nothing -> Nothing


-- Безопасно получи доступ к элементу вложенного списка
safeNested :: Int -> Int -> [[a]] -> Maybe a
safeNested i j xss =
    if i < 0 || j < 0 then Nothing
    else case safeIndex i xss of
        Nothing -> Nothing
        Just row -> safeIndex j row
  where
    safeIndex :: Int -> [b] -> Maybe b
    safeIndex idx lst
        | idx < 0 = Nothing
        | otherwise = case drop idx lst of
            (x:_) -> Just x
            [] -> Nothing




-- Напиши функцию, которая возвращает Right с результатом или Left с сообщением об ошибке
safeDivEither :: Int -> Int -> Either String Int
safeDivEither _ 0 = Left "Division by zero"
safeDivEither x y = Right (x `div` y)


-- Преобразуй Maybe в Either с сообщением об ошибке
maybeToEither :: String -> Maybe a -> Either String a
maybeToEither errorMsg Nothing = Left errorMsg
maybeToEither _ (Just x) = Right x

-- Напиши функцию, которая преобразует Bool в Either
-- True -> Right "Success"
-- False -> Left "Error"
boolToEither :: Bool -> Either String String
boolToEither True = Right "Success"
boolToEither False = Left "Error"


-- Напиши функцию, которая проверяет число
-- Если число положительное -> Right числа
-- Если отрицательное или ноль -> Left "Not positive"
checkPositive :: Int -> Either String Int
checkPositive x = if x > 0 then Right x else Left "Not positive"


-- Напиши функцию, которая возвращает первый элемент списка
-- Если список не пустой -> Right первого элемента
-- Если список пустой -> Left "Empty list"
safeHeadEither :: [a] -> Either String a
safeHeadEither [] = Left "Empty list"
safeHeadEither (x:_) = Right x

filterMaybe3 :: (a -> Bool) -> [Maybe a] -> [Maybe a]
filterMaybe3 pred xs = map f xs
  where
    f (Just x) | pred x    = Just x
               | otherwise = Nothing
    f nothing  = nothing


task :: (a -> c) -> (b -> d) -> [Either a b] -> ([c], [d])
task _ _ [] = ([], [])
task foo boo (x:xs) = case x of
        Left kal -> let (lefts, rights) = task foo boo xs in (foo kal : lefts, rights)
        Right kal2 -> let (lefts, rights) = task foo boo xs in (lefts, boo kal2 : rights)


ddd::(a->b)->(c->d)->[a]->[Maybe c]->[(b,Maybe d)]
ddd _ _ [] _ = []
ddd _ _ _ [] = []
ddd foo boo (x:xs) (y:ys) = case y of
    Just test -> (foo x, Just (boo test)) : ddd foo boo xs ys
    Nothing -> ddd foo boo xs ys

--fff (+1)(*3)[1,2,3][Just 2, Just 4,Nothing]=[(2,Just 6),(3,Just 12)]

polina :: (a -> a -> b) -> [a] -> [a] -> [b]
polina _ [] [] = []
polina f (x:xs) (y:ys) = f x y : polina f xs ys

google :: Maybe (a -> b -> c) -> [Maybe a] -> [Maybe b] -> [Maybe c]
google _ [] _ = []
google _ _ [] = []
google Nothing _ _ = []
google (Just f) (x:xs) (y:ys) = case (x, y) of
    (Just val1, Just val2) -> Just (f val1 val2) : google (Just f) xs ys
    (Nothing, Just val2) -> Nothing : google (Just f) xs ys
    (Just val1, Nothing) -> Nothing : google (Just f) xs ys
    (Nothing, Nothing) -> Nothing : google (Just f) xs ys



mapEither :: (a -> Either e b) -> [a] -> Either e [b]
mapEither _ [] = Right []
mapEither f (x:xs) = case f x of
    Left err -> Left err
    Right y  -> case mapEither f xs of
        Left err -> Left err
        Right ys -> Right (y : ys)


mmEither :: Maybe (Either b b) -> (b -> b) -> Maybe b
mmEither Nothing _ = Nothing
mmEither (Just x) f = case x of
    Right val -> Just (f val)
    Left val -> Just val


factorial2 :: (Eq t, Num t) => t -> t
factorial2 n = if n == 0 then 1 else n * factorial2 (n - 1)

--ccc :: (a -> a -> a) -> [Either a b] -> a -> alab

takeH :: Int -> [a] -> [a]
takeH n _ | n <= 0 = []
takeH _ [] = []
takeH n (x:xs) = x : takeH (n - 1) xs

dropH :: Int -> [a] -> [a]
dropH n xs | n <= 0 = xs
dropH _ [] = []
dropH n (_:xs) = dropH (n - 1) xs

filterH :: (a -> Bool) -> [a] -> [a]
filterH _ [] = []
filterH p (x:xs)
    | p x = x : filterH p xs
    | otherwise = filterH p xs

mapH :: (a -> b) -> [a] -> [b]
mapH _ [] = []
mapH f (x:xs) = f x : map f xs

zipH :: [a] -> [b] -> [(a, b)]
zipH _ [] = []
zipH [] _ = []
zipH (a:as) (b:bs) = (a, b) : zipH as bs 

zipWithH :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithH _ [] _ = []
zipWithH _ _ [] = []
zipWithH f (a:as) (b:bs) = f a b : zipWithH f as bs

combineEitherWith :: (a -> b -> c) -> Either e a -> Either e b -> Either e c
combineEitherWith _ (Left e) _ = Left e
combineEitherWith _  _ (Left e) = Left e
combineEitherWith f (Right x) (Right y) = Right (f x y)

sequenceEither :: [Either e a] -> Either e [a]
sequenceEither [] = Right []
sequenceEither (x:xs) = case x of
    Left err -> Left err
    Right y  -> case sequenceEither xs of
        Left err -> Left err
        Right ys -> Right (y : ys)

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left a) = Left (f a)
mapLeft _ (Right c) = Right c

combineEither :: (a -> b -> c) -> Either e a -> Either e b -> Either e c
combineEither _ _ (Left e) = Left e
combineEither _ (Left e) _ = Left e
combineEither f (Right a) (Right b) = Right (f a b)

example :: Maybe(a -> b) -> [Maybe a] -> Maybe[Maybe b]
example Nothing _ = Nothing
example _ [] = Just []
example (Just f) (x:xs) = case x of
    Nothing -> Nothing
    Just val -> case example (Just f) xs of
        Nothing -> Nothing
        Just rest -> Just (Just (f val) : rest)

may :: Maybe (a -> b) -> Maybe [a] -> Maybe [b]
may Nothing _ = Nothing
may _ Nothing = Nothing
may _ (Just []) = Just []
may (Just f) (Just (x:xs)) = 
    case may (Just f) (Just xs) of
        Just rest -> Just (f x : rest)
        Nothing -> Nothing

boo :: (a -> b -> c) -> [Maybe a] -> [Maybe b] -> [Maybe c]
boo _ [] [] = []
boo _ [] [Just a] = []
boo _ [Just a] [] = []
boo f (x:xs) (y:ys) = case (x, y) of
    (Just value, Nothing) -> boo f xs ys
    (Nothing, Just value) -> boo f xs ys
    (Nothing, Nothing) -> boo f xs ys
    (Just value, Just value2) -> Just (f value value2) : boo f xs ys

bb :: (a -> b) -> [Maybe a] -> [Maybe b]
bb _ [] = []
bb f (x:xs) = case x of
    (Just val) -> (Just (f val)) : bb f xs
    Nothing -> bb f xs

func4 :: (a -> a -> a) -> a -> [Either a a] -> a
func4 _ acc [] = acc
func4 f acc (Left x:xs) = func4 f (f acc x) xs
func4 f acc (Right x:xs) = func4 f (f acc x) xs

myfunc18 :: Either (a -> c) (b -> c) -> [Maybe a] -> [Maybe b] -> [Maybe c]
myfunc18 _ [] _ = []
myfunc18 _ _ [] = []
myfunc18 (Left f) (ma:mas) bs = 
    case ma of
        Just a -> Just (f a) : myfunc18 (Left f) mas bs
        Nothing -> Nothing : myfunc18 (Left f) mas bs
myfunc18 (Right g) as (mb:mbs) = 
    case mb of
        Just b -> Just (g b) : myfunc18 (Right g) as mbs
        Nothing -> Nothing : myfunc18 (Right g) as mbs

foldr3 :: (a ->b -> b) -> b -> [a] -> b 
foldr3 f z [] = z
foldr3 f z (x:xs) = x `f` (foldr3 f z xs)


-- Напиши функцию, которая находит первый элемент, удовлетворяющий условию
findMaybe :: (a -> Bool) -> [a] -> Maybe a
findMaybe _ [] = Nothing
findMaybe pred (x:xs)
    | pred x    = Just x
    | otherwise = findMaybe pred xs

mapMaybe2 :: (a -> Maybe b) -> [a] -> [b]
mapMaybe2 _ [] = []
mapMaybe2 f (x:xs) =
    case f x of
        Just y  -> y : mapMaybe2 f xs
        Nothing -> mapMaybe2 f xs

filterMaybe :: (a -> Bool) -> [Maybe a] -> [Maybe a]
filterMaybe _ [] = []
filterMaybe p (Nothing:xs) = Nothing : filterMaybe p xs
filterMaybe p (Just x:xs)
    | p x       = Just x : filterMaybe p xs
    | otherwise = Nothing : filterMaybe p xs

func5 :: (a -> a -> a) -> (a -> Bool) -> a -> [a] -> a
func5 _ _ acc [] = acc
func5 f p acc (x:xs)
    | p x       = func5 f p (f acc x) xs
    | otherwise = func5 f p acc xs

func5' :: (a -> a -> a) -> (a -> Bool) -> a -> [a] -> a
func5' _ _ acc [] = acc
func5' f p acc (x:xs) = 
    if p x 
    then func5' f p (f acc x) xs
    else func5' f p acc xs

func6 :: [a] -> Maybe (a -> a) -> (a -> a) -> [a]
func6 [] _ _ = []
func6 (x:xs) Nothing g = g x : func6 xs Nothing g
func6 (x:xs) (Just f) g = g (f x) : func6 xs (Just f) g

func7 :: (a -> b) -> Maybe [a] -> Maybe [b]
func7 _ Nothing = Nothing
func7 _ (Just []) = Just[]
func7 f (Just(x:xs)) = 
    case func7 f (Just xs) of
        Just rest -> Just (f x : rest)
        Nothing -> Nothing


fm :: (a -> Bool) -> [Maybe a] -> [Maybe a]
fm _ [] = []
fm f (Nothing:xs) = Nothing : fm f xs
fm f (Just x:xs) 
    | f x       = Just x : fm f xs
    | otherwise = Nothing : fm f xs


fm' :: (a -> Bool) -> [Maybe a] -> [Maybe a]
fm' f [] = []
fm' f (x:xs) = case x of
    Just val -> if f val then Just val : fm' f xs else Nothing : fm' f xs
    Nothing -> Nothing : fm' f xs

func8 :: [a] -> Maybe(a->a) -> (a->a) ->[a]
func8 [] _ _ = []
func8 (x:xs) Nothing f = f x : func8 xs Nothing f
func8 (x:xs) (Just g) f = f (g x) : func8 xs (Just g) f

func9 :: [Maybe a] -> [Bool]
func9 [] = []
func9 (x:xs) = case x of
    Nothing -> False : func9 xs
    Just val -> True : func9 xs
--func9 (Nothing:xs) = False : func9 xs
--func9 (Just _:xs) = True : func9 xs

func10 :: [(b -> b -> b)] -> [[b]] -> [b]
func10 _ [] = []
func10 [] _ = []
func10 (f:fs) (list:lists) = foldl1 f list : func10 fs lists


f5 :: (a -> Bool) -> [a] -> Maybe a
f5 _ [] = Nothing
f5 f (x:xs) = if f x then Just x else f5 f xs
f5 f (x:xs) 
    | f x = Just x
    | otherwise = f5 f xs

f6 :: (a -> Maybe b) -> [a] -> [b]
f6 _ [] = []
f6 f (x:xs) = case f x of
    Just val -> val : f6 f xs
    Nothing -> f6 f xs

f7 :: (a -> a -> a) -> (a -> Bool) -> a -> [a] -> a
f7 _ _ acc [] = acc
f7 f g acc (x:xs) = 
    if g x 
    then f7 f g (f acc x) xs
    else f7 f g acc xs 


f8 :: [a] -> Maybe (a -> a) -> (a -> a) -> [a]
f8 [] _ _ = []
f8 (x:xs) Nothing p = p x : f8 xs Nothing p
f8 (x:xs) (Just func) p = p (func x) : f8 xs (Just func) p

f9 :: (a -> b) -> Maybe [a] -> Maybe [b]
f9 _ Nothing = Nothing
f9 _ (Just []) = Just []
f9 p (Just (x:xs)) = case f9 p (Just xs) of
    Just rest -> Just (p x : rest)
    Nothing -> Nothing

f10 :: (a -> Bool) -> [Maybe a] -> [Maybe a]
f10 _ [] = []
f10 f (Nothing:xs) = Nothing : f10 f xs
f10 f (Just x:xs) 
    | f x       = Just x : f10 f xs
    | otherwise = Nothing : f10 f xs

f4 :: [a] -> Maybe(a->a) -> (a->a) ->[a]
f4 [] _ _ = []
f4 (x:xs) Nothing g = g x : f4 xs Nothing g
f4 (x:xs) (Just f) g = g (f x) : f4 xs (Just f) g

f3 :: [Maybe a] -> [Bool]
f3 [] = []
f3 (Nothing:xs) = False : f3 xs
f3 (Just x:xs) = True : f3 xs

f2 :: [(b -> b -> b)] -> [[b]] -> [b]
f2 _ [] = []
f2 [] _ = []
f2 (f:fs) (list:lists) = foldl1 f list : f2 fs lists

f11 :: (a -> a -> a) -> a -> [Either a a] -> a
f11 _ acc [] = acc
f11 f acc (Left x:xs) = f11 f (f acc x) xs
f11 f acc (Right x:xs) = f11 f (f acc x) xs

f12 :: (a -> b) -> Either a c -> Either b c
f12 f (Left a) = Left (f a)
f12 f (Right c) = Right c

f13 :: Maybe (Either b b) -> (b -> b) -> Maybe b
f13 Nothing _ = Nothing
f13 (Just (Left b)) f = Just b
f13 (Just (Right b)) f = Just (f b)

f14 :: (a -> Either e b) -> [a] -> Either e [b]
f14 _ [] = Right []
f14 f (x:xs) = case f x of
    Left err -> Left err
    Right y -> case f14 f xs of
        Left err -> Left err
        Right ys -> Right (y:ys)

f15 :: [Either e a] -> Either e [a]
f15 [] = Right []
f15 (x:xs) = case x of
    Left err -> Left err
    Right y -> case f15 xs of
        Left err -> Left err
        Right ys -> Right (y:ys)