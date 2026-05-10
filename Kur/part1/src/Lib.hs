{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}

module Lib
    ( parsePNM
    , processImageParallel
    , savePNM
    , PNM(..)
    , Pixel(..)
    , PNMFormat(..)
    , MagicNumber(..)
    , loadPNMFile
    , applyEffect
    , Effect(..)
    ) where

import Control.Applicative ((<|>), empty, many, some, Alternative)
import Data.Word (Word8, Word16)
import Data.Char (isSpace, isDigit, ord, chr)
import Data.Bits (shiftL, shiftR, (.|.), (.&.))
import Data.List (unfoldr, intersperse, groupBy)
import System.IO (Handle, hGetChar, hIsEOF, hPutChar, hPutStr, 
                  withBinaryFile, IOMode(ReadMode, WriteMode))
import Control.Parallel.Strategies (parList, rdeepseq, using)
import Data.Maybe (catMaybes, fromMaybe)
import Control.Monad (guard, replicateM, void)

-- ============ ТИПЫ ДАННЫХ ============

-- Магические числа PNM форматов
data MagicNumber = P1 | P2 | P3 | P4 | P5 | P6
    deriving (Show, Eq, Enum)

-- Формат файла: ASCII или бинарный
data PNMFormat = ASCII MagicNumber | Binary MagicNumber
    deriving (Show, Eq)

-- Пиксель в разных форматах
data Pixel = 
      BW Bool                     -- PBM: True=черный, False=белый
    | Gray Word16                 -- PGM: значение 0-maxVal
    | RGB Word16 Word16 Word16    -- PPM: три компоненты
    deriving (Show, Eq)

-- Полная структура PNM файла
data PNM = PNM {
    pnmFormat :: PNMFormat,    -- Формат файла
    pnmWidth  :: Int,          -- Ширина
    pnmHeight :: Int,          -- Высота
    pnmMaxVal :: Word16,       -- Максимальное значение (1, 255, 65535)
    pnmPixels :: [[Pixel]]     -- Двумерный массив пикселей
} deriving (Show)

-- Эффекты для обработки изображений
data Effect = 
      Invert                     -- Инверсия цветов
    | Threshold Word16           -- Пороговое преобразование
    | Brightness Int             -- Изменение яркости
    | Contrast Double            -- Изменение контрастности
    | FlipHorizontal             -- Отражение по горизонтали
    | FlipVertical               -- Отражение по вертикали
    | Grayscale                  -- Преобразование в оттенки серого
    deriving (Show)


-- ============ СВОЙ ПАРСЕР БЕЗ ВНЕШНИХ ЗАВИСИМОСТЕЙ ============

-- Тип парсера
newtype Parser a = Parser {
    runParser :: String -> Maybe (a, String)
}

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f (Parser p) = Parser $ \s -> case p s of
        Just (x, s') -> Just (f x, s')
        Nothing -> Nothing

instance Applicative Parser where
    pure :: a -> Parser a
    pure x = Parser $ \s -> Just (x, s)
    
    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    Parser pf <*> Parser px = Parser $ \s -> case pf s of
        Just (f, s') -> case px s' of
            Just (x, s'') -> Just (f x, s'')
            Nothing -> Nothing
        Nothing -> Nothing

instance Alternative Parser where
    empty :: Parser a
    empty = Parser $ \_ -> Nothing
    
    (<|>) :: Parser a -> Parser a -> Parser a
    Parser p1 <|> Parser p2 = Parser $ \s -> p1 s <|> p2 s

instance Monad Parser where
    return = pure
    Parser p >>= f = Parser $ \s -> case p s of
        Just (x, s') -> runParser (f x) s'
        Nothing -> Nothing

-- Базовые комбинаторы
char :: Char -> Parser Char
char c = Parser $ \s -> case s of
    (x:xs) | x == c -> Just (c, xs)
    _ -> Nothing

string :: String -> Parser String
string = traverse char

decimal :: Parser Int
decimal = do
    digits <- some (satisfy isDigit)
    case reads digits of
        [(n, _)] -> return n
        _ -> empty

word16 :: Parser Word16
word16 = fromIntegral <$> decimal

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \s -> case s of
    (x:xs) | p x -> Just (x, xs)
    _ -> Nothing

skipSpace :: Parser ()
skipSpace = Parser $ \s -> Just ((), dropWhile isSpace s)

skipWhile :: (Char -> Bool) -> Parser ()
skipWhile p = Parser $ \s -> Just ((), dropWhile p s)

skipMany :: Parser a -> Parser ()
skipMany p = many p *> pure ()

skipSome :: Parser a -> Parser ()
skipSome p = some p *> pure ()

optional :: Parser a -> Parser (Maybe a)
optional p = (Just <$> p) <|> pure Nothing

choice :: [Parser a] -> Parser a
choice = foldr (<|>) empty

count :: Int -> Parser a -> Parser [a]
count n p
    | n <= 0    = pure []
    | otherwise = (:) <$> p <*> count (n-1) p

skipWhitespace :: Parser ()
skipWhitespace = do
    skipSpace
    optional $ do
        char '#'
        skipWhile (/= '\n')
        skipWhitespace
    return ()

-- Парсер магического числа
parseMagicNumber :: Parser MagicNumber
parseMagicNumber = choice
    [ P1 <$ string "P1"
    , P2 <$ string "P2"
    , P3 <$ string "P3"
    , P4 <$ string "P4"
    , P5 <$ string "P5"
    , P6 <$ string "P6"
    ]

-- ============ ПАРСЕРЫ ASCII ФОРМАТОВ ============

-- Парсер ASCII пикселя PBM (P1)
parseP1Pixel :: Parser Pixel
parseP1Pixel = do
    skipWhitespace
    n <- decimal
    case n of
        0 -> return $ BW False
        1 -> return $ BW True
        _ -> empty

-- Парсер ASCII пикселя PGM (P2)
parseP2Pixel :: Word16 -> Parser Pixel
parseP2Pixel maxVal = do
    skipWhitespace
    val <- word16
    guard (val <= maxVal)
    return $ Gray val

-- Парсер ASCII пикселя PPM (P3)
parseP3Pixel :: Word16 -> Parser Pixel
parseP3Pixel maxVal = do
    skipWhitespace
    r <- word16
    guard (r <= maxVal)
    skipWhitespace
    g <- word16
    guard (g <= maxVal)
    skipWhitespace
    b <- word16
    guard (b <= maxVal)
    return $ RGB r g b

-- Парсер заголовка PNM
parseHeader :: Parser (PNMFormat, Int, Int, Word16)
parseHeader = do
    skipWhitespace
    magic <- parseMagicNumber
    
    skipWhitespace
    width <- decimal
    guard (width > 0)
    
    skipWhitespace
    height <- decimal
    guard (height > 0)
    
    -- Для P1 и P4 maxVal всегда 1, для других нужно прочитать
    maxVal <- case magic of
        P1 -> return 1
        P4 -> return 1
        _ -> do
            skipWhitespace
            mv <- word16
            guard (mv > 0 && mv <= 65535)
            return mv
    
    let format = case magic of
            P1 -> ASCII P1
            P2 -> ASCII P2
            P3 -> ASCII P3
            P4 -> Binary P4
            P5 -> Binary P5
            P6 -> Binary P6
    
    return (format, width, height, maxVal)

-- ============ РАБОТА С БИНАРНЫМИ ДАННЫМИ ============

-- Чтение Word8 из строки (для бинарных данных)
readWord8 :: String -> Maybe (Word8, String)
readWord8 [] = Nothing
readWord8 (c:cs) = Just (fromIntegral (ord c), cs)

-- Парсер Word8 для бинарных данных
anyWord8 :: Parser Word8
anyWord8 = Parser readWord8

-- Чтение Word16 (2 байта big-endian)
anyWord16 :: Parser Word16
anyWord16 = do
    h <- anyWord8
    l <- anyWord8
    return $ (fromIntegral h `shiftL` 8) .|. fromIntegral l

-- Парсер бинарного пикселя PBM (P4) - 1 бит на пиксель
parseP4Pixel :: Int -> Parser [Pixel]
parseP4Pixel bitsInByte = do
    byte <- anyWord8
    let pixels = [ BW (testBit byte (7 - i)) | i <- [0..bitsInByte-1] ]
    return pixels

-- Проверка бита
testBit :: Word8 -> Int -> Bool
testBit byte n = (byte `shiftR` n) .&. 1 == 1

-- Парсер бинарного пикселя PGM (P5)
parseP5Pixel :: Word16 -> Parser Pixel
parseP5Pixel maxVal
    | maxVal <= 255 = Gray . fromIntegral <$> anyWord8
    | otherwise = Gray <$> anyWord16

-- Парсер бинарного пикселя PPM (P6)
parseP6Pixel :: Word16 -> Parser Pixel
parseP6Pixel maxVal
    | maxVal <= 255 = do
        r <- anyWord8
        g <- anyWord8
        b <- anyWord8
        return $ RGB (fromIntegral r) (fromIntegral g) (fromIntegral b)
    | otherwise = do
        r <- anyWord16
        g <- anyWord16
        b <- anyWord16
        return $ RGB r g b

-- ============ ОСНОВНОЙ ПАРСЕР PNM ============

-- Парсер данных изображения
-- Парсер данных изображения
parseImageData :: PNMFormat -> Int -> Int -> Word16 -> Parser [[Pixel]]
parseImageData format width height maxVal = do
    let totalPixels = width * height
    
    pixelsFlat <- case format of
        ASCII P1 -> count totalPixels parseP1Pixel
        ASCII P2 -> count totalPixels (parseP2Pixel maxVal)
        ASCII P3 -> count totalPixels (parseP3Pixel maxVal)
        
        Binary P4 -> do
            let bytesNeeded = (totalPixels + 7) `div` 8
            bits <- concat <$> count bytesNeeded (parseP4Pixel 8)
            return $ take totalPixels bits
        
        Binary P5 -> count totalPixels (parseP5Pixel maxVal)
            
        Binary P6 -> count totalPixels (parseP6Pixel maxVal)
    
    -- Возвращаем уже разбитый на строки список
    return $ chunksOf width pixelsFlat

-- Разделение списка на блоки
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- Полный парсер PNM файла
parsePNM :: String -> Either String PNM
parsePNM input = case runParser parser input of
    Just (pnm, "") -> Right pnm
    Just (_, rest) -> Left $ "Unexpected data at end: " ++ take 50 rest
    Nothing -> Left "Parse error"
  where
    parser = do
        (format, width, height, maxVal) <- parseHeader
        skipWhitespace
        
        pixels <- parseImageData format width height maxVal
        
        return $ PNM format width height maxVal pixels

-- ============ ЗАГРУЗКА И СОХРАНЕНИЕ ФАЙЛОВ ============

-- Загрузка PNM файла
loadPNMFile :: FilePath -> IO (Either String PNM)
loadPNMFile filepath = do
    content <- readFile filepath
    return $ parsePNM content

-- Сохранение PNM файла
savePNM :: FilePath -> PNM -> IO ()
savePNM filepath pnm = do
    let content = serializePNM pnm
    writeFile filepath content

-- Сериализация PNM в строку
serializePNM :: PNM -> String
serializePNM PNM{..} = 
    let magicStr = case pnmFormat of
            ASCII P1 -> "P1"
            ASCII P2 -> "P2"
            ASCII P3 -> "P3"
            Binary P4 -> "P4"
            Binary P5 -> "P5"
            Binary P6 -> "P6"
        
        header = unlines 
            [ magicStr
            , show pnmWidth ++ " " ++ show pnmHeight
            , if pnmMaxVal == 1 then "" else show pnmMaxVal
            ]
        
        pixelData = case pnmFormat of
            ASCII P1 -> serializeASCIIPBM pnmPixels
            ASCII P2 -> serializeASCIIPGM pnmMaxVal pnmPixels
            ASCII P3 -> serializeASCIIPPM pnmMaxVal pnmPixels
            Binary P4 -> serializeBinaryPBM pnmPixels
            Binary P5 -> serializeBinaryPGM pnmMaxVal pnmPixels
            Binary P6 -> serializeBinaryPPM pnmMaxVal pnmPixels
    in header ++ pixelData

-- Сериализация ASCII PBM
serializeASCIIPBM :: [[Pixel]] -> String
serializeASCIIPBM pixels = unlines $ map (unwords . map pixelToPBM) pixels
  where
    pixelToPBM (BW True) = "1"
    pixelToPBM (BW False) = "0"
    pixelToPBM _ = error "Invalid pixel type for PBM"

-- Сериализация ASCII PGM
serializeASCIIPGM :: Word16 -> [[Pixel]] -> String
serializeASCIIPGM maxVal pixels = unlines $ map (unwords . map pixelToPGM) pixels
  where
    pixelToPGM (Gray val) = show val
    pixelToPGM _ = error "Invalid pixel type for PGM"

-- Сериализация ASCII PPM
serializeASCIIPPM :: Word16 -> [[Pixel]] -> String
serializeASCIIPPM maxVal pixels = unlines $ map (unwords . map pixelToPPM) pixels
  where
    pixelToPPM (RGB r g b) = show r ++ " " ++ show g ++ " " ++ show b
    pixelToPPM _ = error "Invalid pixel type for PPM"

-- Сериализация бинарного PBM
serializeBinaryPBM :: [[Pixel]] -> String
serializeBinaryPBM pixels = 
    let bits = concatMap ((\x -> [x]) . pixelToBit) (concat pixels)  -- оборачиваем Bool в список
        bytes = chunksOf 8 bits
        byteToChar bits8 = chr $ foldl (\acc (i, bit) -> 
            if bit then acc .|. (1 `shiftL` (7-i)) else acc) 0 (zip [0..7] bits8)
    in map byteToChar bytes
  where
    pixelToBit :: Pixel -> Bool
    pixelToBit (BW True) = True
    pixelToBit (BW False) = False
    pixelToBit _ = error "Invalid pixel type for PBM"

-- Сериализация бинарного PGM
serializeBinaryPGM :: Word16 -> [[Pixel]] -> String
serializeBinaryPGM maxVal pixels
    | maxVal <= 255 = concatMap ((\x -> [x]) . chr . fromIntegral . grayToWord8) (concat pixels)
    | otherwise = concatMap grayToWord16Str (concat pixels)
  where
    grayToWord8 :: Pixel -> Word8
    grayToWord8 (Gray val) = fromIntegral val
    grayToWord8 _ = error "Invalid pixel type for PGM"
    
    grayToWord16Str :: Pixel -> String
    grayToWord16Str (Gray val) = 
        let hi = chr (fromIntegral (val `shiftR` 8))
            lo = chr (fromIntegral (val .&. 0xFF))
        in [hi, lo]
    grayToWord16Str _ = error "Invalid pixel type for PGM"

-- Сериализация бинарного PPM
serializeBinaryPPM :: Word16 -> [[Pixel]] -> String
serializeBinaryPPM maxVal pixels
    | maxVal <= 255 = concatMap rgbToWord8Str (concat pixels)
    | otherwise = concatMap rgbToWord16Str (concat pixels)
  where
    rgbToWord8Str :: Pixel -> String
    rgbToWord8Str (RGB r g b) = 
        map (chr . fromIntegral) [fromIntegral r, fromIntegral g, fromIntegral b]
    rgbToWord8Str _ = error "Invalid pixel type for PPM"
    
    rgbToWord16Str :: Pixel -> String
    rgbToWord16Str (RGB r g b) = 
        let toCharPair val = 
                let hi = chr (fromIntegral (val `shiftR` 8))
                    lo = chr (fromIntegral (val .&. 0xFF))
                in [hi, lo]
        in toCharPair r ++ toCharPair g ++ toCharPair b
    rgbToWord16Str _ = error "Invalid pixel type for PPM"

-- ============ ОБРАБОТКА ИЗОБРАЖЕНИЙ ============

-- Тяжелые вычисления (для демонстрации параллелизма)
heavyComputation :: Pixel -> Pixel
heavyComputation pixel = case pixel of
    BW b -> BW (not b)  -- Инверсия для PBM
    Gray val -> Gray (65535 - val)  -- Инверсия для PGM
    RGB r g b -> RGB (65535 - r) (65535 - g) (65535 - b)  -- Инверсия для PPM

processImageParallel :: PNM -> PNM
processImageParallel pnm@PNM{..} = 
    let processedRows = map (map heavyComputation) pnmPixels
    in pnm { pnmPixels = processedRows }

-- Применение эффекта к изображению
applyEffect :: Effect -> PNM -> PNM
applyEffect effect pnm@PNM{..} = 
    let transform = case effect of
            Invert -> invertPixel
            Threshold t -> thresholdPixel t
            Brightness delta -> brightnessPixel delta
            Contrast factor -> contrastPixel factor
            FlipHorizontal -> id  -- Просто функция identity
            FlipVertical -> id    -- Просто функция identity
            Grayscale -> toGrayscale
        
        -- Применяем трансформацию к пикселям
        newPixels = case effect of
            FlipHorizontal -> map reverse pnmPixels
            FlipVertical -> reverse pnmPixels
            _ -> map (map transform) pnmPixels
    in pnm { pnmPixels = newPixels }

-- Инверсия пикселя
invertPixel :: Pixel -> Pixel
invertPixel (BW b) = BW (not b)
invertPixel (Gray val) = Gray (65535 - val)  -- Используем максимальное значение для Word16
invertPixel (RGB r g b) = RGB (65535 - r) (65535 - g) (65535 - b)

-- Пороговое преобразование
thresholdPixel :: Word16 -> Pixel -> Pixel
thresholdPixel threshold pixel = 
    let brightness = pixelBrightness pixel
    in if brightness > threshold 
        then BW False  -- Белый (False в PBM = белый)
        else BW True   -- Черный (True в PBM = черный)

-- Яркость пикселя
brightnessPixel :: Int -> Pixel -> Pixel
brightnessPixel delta pixel = case pixel of
    BW b -> BW b  -- Для PBM не меняем
    Gray val -> Gray (clampWord16 (fromIntegral val + delta))
    RGB r g b -> RGB (clampWord16 (fromIntegral r + delta))
                     (clampWord16 (fromIntegral g + delta))
                     (clampWord16 (fromIntegral b + delta))

-- Контрастность
contrastPixel :: Double -> Pixel -> Pixel
contrastPixel factor pixel = case pixel of
    BW b -> BW b
    Gray val -> 
        let normalized = fromIntegral val / 255.0
            adjusted = 0.5 + factor * (normalized - 0.5)
            newVal = round (adjusted * 255.0)
        in Gray (clampWord16 newVal)
    RGB r g b ->
        let normalize x = fromIntegral x / 255.0
            adjust x = 0.5 + factor * (x - 0.5)
            newR = round (adjust (normalize r) * 255.0)
            newG = round (adjust (normalize g) * 255.0)
            newB = round (adjust (normalize b) * 255.0)
        in RGB (clampWord16 newR) (clampWord16 newG) (clampWord16 newB)

-- Преобразование в оттенки серого
toGrayscale :: Pixel -> Pixel
toGrayscale pixel = case pixel of
    BW b -> Gray (if b then 0 else 255)
    Gray val -> Gray val
    RGB r g b -> 
        let gray = round (0.299 * fromIntegral r + 0.587 * fromIntegral g + 0.114 * fromIntegral b)
        in Gray (fromIntegral gray)

-- Яркость пикселя (0-255)
pixelBrightness :: Pixel -> Word16
pixelBrightness (BW True) = 0
pixelBrightness (BW False) = 255
pixelBrightness (Gray val) = fromIntegral val
pixelBrightness (RGB r g b) = 
    round (0.299 * fromIntegral r + 0.587 * fromIntegral g + 0.114 * fromIntegral b)

-- Ограничение значения Word16
clampWord16 :: Int -> Word16
clampWord16 x
    | x < 0 = 0
    | x > 65535 = 65535
    | otherwise = fromIntegral x

-- ============ ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ ============

-- Пример PBM файла для тестирования
examplePBM :: String
examplePBM = unlines
    [ "P1"
    , "# Пример PBM файла"
    , "4 4"
    , "0 1 0 1"
    , "1 0 1 0"
    , "0 1 0 1"
    , "1 0 1 0"
    ]

-- Пример PGM файла для тестирования
examplePGM :: String
examplePGM = unlines
    [ "P2"
    , "# Пример PGM файла"
    , "3 2"
    , "255"
    , "50 100 150"
    , "200 250 0"
    ]

-- Пример PPM файла для тестирования
examplePPM :: String
examplePPM = unlines
    [ "P3"
    , "# Пример PPM файла"
    , "2 2"
    , "255"
    , "255 0 0 0 255 0"
    , "0 0 255 255 255 255"
    ]

-- Тестирование парсера
testParser :: IO ()
testParser = do
    putStrLn "Тестирование парсера PBM:"
    case parsePNM examplePBM of
        Left err -> putStrLn $ "Ошибка: " ++ err
        Right pnm -> print pnm
    
    putStrLn "\nТестирование парсера PGM:"
    case parsePNM examplePGM of
        Left err -> putStrLn $ "Ошибка: " ++ err
        Right pnm -> print pnm
    
    putStrLn "\nТестирование парсера PPM:"
    case parsePNM examplePPM of
        Left err -> putStrLn $ "Ошибка: " ++ err
        Right pnm -> print pnm