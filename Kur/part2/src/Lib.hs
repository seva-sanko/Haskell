{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

module Lib
    ( 
      BMP(..)
    , BMPHeader(..)
    , DIBHeader(..)
    , PixelData(..)
    , Color(..)
    
    , ChromaKey(..)
    , defaultGreenScreen
    , defaultBlueScreen
    
    , Effect(..)
    , createSharpenKernel
    , createGaussianKernel
    
    , Composition(..)
    , Layer(..)
    , BlendMode(..)
    
    , parseBMP
    , saveBMP
    , loadBMPFile
    , applyEffect
    , chromaKeyMask
    , renderComposition
    
    , createRandomDisplacement
    , calculateBrightness
    , calculateContrast
    
    ) where

import Data.Word (Word8, Word16, Word32)
import Data.Bits (shiftL, shiftR, (.|.), (.&.), complement, xor)
import Data.Char (ord, chr)
import System.IO (withBinaryFile, IOMode(..), hGetBuf, hPutBuf, hPutStrLn)
import System.Random (RandomGen, randomR, split, mkStdGen, StdGen, randomRs, randoms)
import Control.Monad (when, replicateM, forM_, guard)
import Data.List (transpose, foldl', sortBy, groupBy)
import Data.Maybe (fromMaybe, catMaybes)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Control.Applicative ((<|>))
import Data.Int (Int32)
import qualified Data.Vector as V


data Color = Color
    { red   :: Word8
    , green :: Word8
    , blue  :: Word8
    , alpha :: Word8  
    } deriving (Show, Eq)

instance Semigroup Color where
    Color r1 g1 b1 a1 <> Color r2 g2 b2 a2 =
        Color (mix r1 r2) (mix g1 g2) (mix b1 b2) (mix a1 a2)
      where
        mix x y = fromIntegral ((fromIntegral x + fromIntegral y) `div` 2)

instance Monoid Color where
    mempty = Color 0 0 0 0

data BMPHeader = BMPHeader
    { bmpSignature      :: Word16  
    , bmpFileSize       :: Word32
    , bmpReserved1      :: Word16
    , bmpReserved2      :: Word16
    , bmpPixelOffset    :: Word32 
    } deriving (Show, Eq)


data DIBHeader = DIBHeader
    { dibHeaderSize     :: Word32  -- Размер этого заголовка (40 для BITMAPINFOHEADER)
    , dibWidth          :: Int32   -- Ширина в пикселях
    , dibHeight         :: Int32   -- Высота в пикселях (отрицательный = top-down)
    , dibPlanes         :: Word16  -- Всегда 1
    , dibBitsPerPixel   :: Word16  -- 1, 4, 8, 16, 24, 32
    , dibCompression    :: Word32  -- 0 = BI_RGB (без сжатия)
    , dibImageSize      :: Word32  -- Размер данных изображения (может быть 0 для BI_RGB)
    , dibXPixelsPerM    :: Int32   -- Горизонтальное разрешение
    , dibYPixelsPerM    :: Int32   -- Вертикальное разрешение
    , dibColorsUsed     :: Word32  -- Число используемых цветов в палитре
    , dibColorsImportant :: Word32 -- Число важных цветов
    } deriving (Show, Eq)

data PixelData
    = Monochrome [[Bool]]                    -- 1 бит на пиксель
    | Indexed4 [[Word8]]                     -- 4 бита на пиксель
    | Indexed8 [[Word8]] [[Word8]] [[Word8]] -- 8 бит на пиксель (R, G, B)
    | RGB16 [[Word16]]                       -- 16 бит (обычно 5-6-5)
    | RGB24 [[Color]]                        -- 24 бита (8-8-8)
    | RGB32 [[Color]]                        -- 32 бита (8-8-8-8)
    deriving (Show, Eq)


data BMP = BMP
    { bmpHeader   :: BMPHeader
    , dibHeader   :: DIBHeader
    , colorTable  :: Maybe [Color] 
    , pixelData   :: PixelData
    } deriving (Show, Eq)


data ChromaKey = ChromaKey
    { targetColor  :: Color
    , toleranceR   :: Word8
    , toleranceG   :: Word8
    , toleranceB   :: Word8
    , softEdge     :: Word8  
    } deriving (Show, Eq)

defaultGreenScreen :: ChromaKey
defaultGreenScreen = ChromaKey
    { targetColor = Color 0 255 0 255
    , toleranceR  = 50
    , toleranceG  = 50
    , toleranceB  = 50
    , softEdge    = 10
    }

defaultBlueScreen :: ChromaKey
defaultBlueScreen = ChromaKey
    { targetColor = Color 0 0 255 255
    , toleranceR  = 50
    , toleranceG  = 50
    , toleranceB  = 50
    , softEdge    = 10
    }


data Effect
    = Invert                     -- Инверсия цвета
    | Grayscale                  -- Ч/Б
    | Brightness Int             -- Яркость (-255..255)
    | Contrast Double            -- Контрастность (0.0..3.0)
    | FlipHorizontal             -- Отражение по горизонтали
    | FlipVertical               -- Отражение по вертикали
    | Sharpen                    -- Усиление резкости
    | GaussianBlur Int           -- Размытие по Гауссу (радиус)
    | Threshold Word8            -- Пороговое преобразование
    | GlitchEffect StdGen        -- Эффект глитча (с генератором случайных чисел)
    | CustomKernel [[Double]]    -- Пользовательское ядро свертки
    deriving (Show)

-- Ядро для усиления резкости
createSharpenKernel :: [[Double]]
createSharpenKernel =
    [ [ 0, -1,  0]
    , [-1,  5, -1]
    , [ 0, -1,  0]
    ]

createGaussianKernel :: [[Double]]
createGaussianKernel =
    [ [1, 2, 1]
    , [2, 4, 2]
    , [1, 2, 1]
    ] >>= (return . map (/16))


data BlendMode
    = Normal       -- Обычное наложение
    | Multiply     -- Умножение
    | Screen       -- Экран
    | Overlay      -- Перекрытие
    | Additive     -- Аддитивное
    deriving (Show, Eq, Enum)

data Layer = Layer
    { layerImage     :: BMP
    , layerChromaKey :: Maybe ChromaKey  
    , layerEffects   :: [Effect]
    , layerBlendMode :: BlendMode
    , layerOpacity   :: Double
    , layerPosition  :: (Int, Int)
    } deriving (Show)

data Composition = Composition
    { compWidth     :: Int
    , compHeight    :: Int
    , compBackground :: Color
    , compLayers    :: [Layer]
    } deriving (Show)


readWord16LE :: B.ByteString -> Int -> Word16
readWord16LE bs offset =
    let b1 = fromIntegral (B.index bs offset) :: Word16
        b2 = fromIntegral (B.index bs (offset + 1)) :: Word16
    in b1 .|. (b2 `shiftL` 8)

readWord32LE :: B.ByteString -> Int -> Word32
readWord32LE bs offset =
    let b1 = fromIntegral (B.index bs offset) :: Word32
        b2 = fromIntegral (B.index bs (offset + 1)) :: Word32
        b3 = fromIntegral (B.index bs (offset + 2)) :: Word32
        b4 = fromIntegral (B.index bs (offset + 3)) :: Word32
    in b1 .|. (b2 `shiftL` 8) .|. (b3 `shiftL` 16) .|. (b4 `shiftL` 24)

readInt32LE :: B.ByteString -> Int -> Int32
readInt32LE bs offset =
    let w32 = readWord32LE bs offset
    in fromIntegral (fromIntegral w32 :: Int32)

parseBMPHeader :: B.ByteString -> Maybe (BMPHeader, Int)
parseBMPHeader bs = do
    guard (B.length bs >= 14)
    let signature = readWord16LE bs 0
    guard (signature == 0x4D42)  
    
    let fileSize = readWord32LE bs 2
    let reserved1 = readWord16LE bs 6
    let reserved2 = readWord16LE bs 8
    let pixelOffset = readWord32LE bs 10
    
    return (BMPHeader signature fileSize reserved1 reserved2 pixelOffset, 14)
  where
    guard = Control.Monad.guard

parseDIBHeader :: B.ByteString -> Int -> Maybe (DIBHeader, Int)
parseDIBHeader bs offset = do
    guard (B.length bs >= offset + 40)
    let headerSize = readWord32LE bs offset
    let width = readInt32LE bs (offset + 4)
    let height = readInt32LE bs (offset + 8)
    let planes = readWord16LE bs (offset + 12)
    let bitsPerPixel = readWord16LE bs (offset + 14)
    let compression = readWord32LE bs (offset + 16)
    let imageSize = readWord32LE bs (offset + 20)
    let xPixelsPerM = readInt32LE bs (offset + 24)
    let yPixelsPerM = readInt32LE bs (offset + 28)
    let colorsUsed = readWord32LE bs (offset + 32)
    let colorsImportant = readWord32LE bs (offset + 36)
    
    return (DIBHeader headerSize width height planes bitsPerPixel compression
                      imageSize xPixelsPerM yPixelsPerM colorsUsed colorsImportant,
            offset + fromIntegral headerSize)
  where
    guard = Control.Monad.guard

readColorTable :: B.ByteString -> Int -> Word32 -> Maybe ([Color], Int)
readColorTable bs offset numColors = do
    guard (B.length bs >= offset + fromIntegral (numColors * 4))
    let colors = [ Color (B.index bs (offset + i*4 + 2))
                         (B.index bs (offset + i*4 + 1))
                         (B.index bs (offset + i*4))
                         (if B.length bs > offset + i*4 + 3 then B.index bs (offset + i*4 + 3) else 255)
                 | i <- [0..fromIntegral numColors - 1] ]
    return (colors, offset + fromIntegral (numColors * 4))
  where
    guard = Control.Monad.guard

readMonochromeData :: B.ByteString -> Int -> Int -> Int -> Maybe [[Bool]]
readMonochromeData bs offset width height = do
    let rowSize = ((width + 31) `div` 32) * 4  
    let totalSize = rowSize * abs height
    guard (B.length bs >= offset + totalSize)
    
    let rows = [ [ testBit (B.index bs (offset + y*rowSize + x `div` 8)) (7 - (x `mod` 8))
                 | x <- [0..width-1] ]
               | y <- [0..abs height-1] ]
    
    return $ if height < 0 then rows else reverse rows
  where
    guard = Control.Monad.guard
    testBit byte pos = (byte `shiftR` pos) .&. 1 == 1

readRGB24Data :: B.ByteString -> Int -> Int -> Int -> Maybe [[Color]]
readRGB24Data bs offset width height = do
    let rowSize = (width * 3 + 3) `div` 4 * 4 
    let totalSize = rowSize * abs height
    guard (B.length bs >= offset + totalSize)
    
    let rows = [ [ Color (B.index bs (offset + y*rowSize + x*3 + 2))
                         (B.index bs (offset + y*rowSize + x*3 + 1))
                         (B.index bs (offset + y*rowSize + x*3))
                         255
                 | x <- [0..width-1] ]
               | y <- [0..abs height-1] ]
    
    return $ if height < 0 then rows else reverse rows
  where
    guard = Control.Monad.guard

readRGB32Data :: B.ByteString -> Int -> Int -> Int -> Maybe [[Color]]
readRGB32Data bs offset width height = do
    let rowSize = width * 4  
    let totalSize = rowSize * abs height
    guard (B.length bs >= offset + totalSize)
    
    let rows = [ [ Color (B.index bs (offset + y*rowSize + x*4 + 2))
                         (B.index bs (offset + y*rowSize + x*4 + 1))
                         (B.index bs (offset + y*rowSize + x*4))
                         (B.index bs (offset + y*rowSize + x*4 + 3))
                 | x <- [0..width-1] ]
               | y <- [0..abs height-1] ]
    
    return $ if height < 0 then rows else reverse rows
  where
    guard = Control.Monad.guard

parseBMP :: B.ByteString -> Maybe BMP
parseBMP bs = do
    (bmpHeader, offset1) <- parseBMPHeader bs
    (dibHeader, offset2) <- parseDIBHeader bs offset1
    
    let width = fromIntegral $ dibWidth dibHeader
    let height = fromIntegral $ dibHeight dibHeader
    let bitsPerPixel = dibBitsPerPixel dibHeader
    let colorsUsed = dibColorsUsed dibHeader
    

    (colorTable, offset3) <- case bitsPerPixel of
        1  -> case readColorTable bs offset2 (if colorsUsed == 0 then 2 else colorsUsed) of
                Just (colors, offset) -> return (Just colors, offset)
                Nothing -> return (Nothing, offset2)
        4  -> case readColorTable bs offset2 (if colorsUsed == 0 then 16 else colorsUsed) of
                Just (colors, offset) -> return (Just colors, offset)
                Nothing -> return (Nothing, offset2)
        8  -> case readColorTable bs offset2 (if colorsUsed == 0 then 256 else colorsUsed) of
                Just (colors, offset) -> return (Just colors, offset)
                Nothing -> return (Nothing, offset2)
        _  -> return (Nothing, offset2)
    
    let pixelOffset = fromIntegral $ bmpPixelOffset bmpHeader
    
    pixelData <- case bitsPerPixel of
        1  -> Monochrome <$> readMonochromeData bs pixelOffset width height
        24 -> RGB24 <$> readRGB24Data bs pixelOffset width height
        32 -> RGB32 <$> readRGB32Data bs pixelOffset width height
        _  -> Nothing 
    
    return BMP
        { bmpHeader = bmpHeader
        , dibHeader = dibHeader
        , colorTable = colorTable
        , pixelData = pixelData
        }

loadBMPFile :: FilePath -> IO (Maybe BMP)
loadBMPFile filepath = do
    bs <- B.readFile filepath
    return $ parseBMP bs

writeWord16LE :: Word16 -> B.ByteString
writeWord16LE w = B.pack [fromIntegral w, fromIntegral (w `shiftR` 8)]

writeWord32LE :: Word32 -> B.ByteString
writeWord32LE w = B.pack [ fromIntegral w
                         , fromIntegral (w `shiftR` 8)
                         , fromIntegral (w `shiftR` 16)
                         , fromIntegral (w `shiftR` 24) ]

writeInt32LE :: Int32 -> B.ByteString
writeInt32LE i = writeWord32LE (fromIntegral i)

serializeBMPHeader :: BMPHeader -> B.ByteString
serializeBMPHeader BMPHeader{..} =
    B.concat [ writeWord16LE bmpSignature
             , writeWord32LE bmpFileSize
             , writeWord16LE bmpReserved1
             , writeWord16LE bmpReserved2
             , writeWord32LE bmpPixelOffset ]

serializeDIBHeader :: DIBHeader -> B.ByteString
serializeDIBHeader DIBHeader{..} =
    B.concat [ writeWord32LE dibHeaderSize
             , writeInt32LE dibWidth
             , writeInt32LE dibHeight
             , writeWord16LE dibPlanes
             , writeWord16LE dibBitsPerPixel
             , writeWord32LE dibCompression
             , writeWord32LE dibImageSize
             , writeInt32LE dibXPixelsPerM
             , writeInt32LE dibYPixelsPerM
             , writeWord32LE dibColorsUsed
             , writeWord32LE dibColorsImportant ]

serializeColor :: Bool -> Color -> B.ByteString
serializeColor withAlpha Color{..} =
    if withAlpha
    then B.pack [blue, green, red, alpha]
    else B.pack [blue, green, red]

serializeRGB24Data :: [[Color]] -> B.ByteString
serializeRGB24Data rows =
    let height = length rows
        width = length (head rows)
        rowSize = (width * 3 + 3) `div` 4 * 4
        padding = B.replicate (rowSize - width * 3) 0
        
        serializeRow row = B.concat
            [ B.concat (map (serializeColor False) row)
            , padding
            ]
    in B.concat (map serializeRow (reverse rows))  

serializeRGB32Data :: [[Color]] -> B.ByteString
serializeRGB32Data rows =
    let serializeRow = B.concat . map (serializeColor True)
    in B.concat (map serializeRow (reverse rows))

updateFileSize :: BMP -> BMP
updateFileSize bmp@BMP{..} = 
    let dib = dibHeader
        width = fromIntegral $ dibWidth dib
        height = abs $ fromIntegral $ dibHeight dib
        bitsPerPixel = dibBitsPerPixel dib
        
        pixelDataSize = case pixelData of
            RGB24 rows -> (width * 3 + 3) `div` 4 * 4 * height
            RGB32 rows -> width * height * 4
            _ -> 0
        
        colorTableSize = case colorTable of
            Just colors -> length colors * 4
            Nothing -> 0
        
        totalSize = 14 + fromIntegral (dibHeaderSize dib) + colorTableSize + pixelDataSize
    in bmp { bmpHeader = bmpHeader { bmpFileSize = fromIntegral totalSize } }

saveBMP :: FilePath -> BMP -> IO ()
saveBMP filepath bmp = do
    let updatedBMP = updateFileSize bmp
    let bs = serializeBMP updatedBMP
    B.writeFile filepath bs
  where
    serializeBMP BMP{..} =
        B.concat [ serializeBMPHeader bmpHeader
                 , serializeDIBHeader dibHeader
                 , maybe B.empty (B.concat . map (serializeColor False)) colorTable
                 , case pixelData of
                     RGB24 rows -> serializeRGB24Data rows
                     RGB32 rows -> serializeRGB32Data rows
                     _ -> B.empty  
                 ]


colorDistance :: Color -> Color -> Double
colorDistance c1 c2 =
    let dr = fromIntegral (red c1) - fromIntegral (red c2)
        dg = fromIntegral (green c1) - fromIntegral (green c2)
        db = fromIntegral (blue c1) - fromIntegral (blue c2)
    in sqrt (dr*dr + dg*dg + db*db)

chromaKeyMask :: ChromaKey -> [[Color]] -> [[Double]]
chromaKeyMask ChromaKey{..} pixels =
    let target = targetColor
        maxDist = sqrt ( fromIntegral (toleranceR^2 + toleranceG^2 + toleranceB^2) )
        soft = fromIntegral softEdge
        
        calculateAlpha pixel =
            let dist = colorDistance pixel target
                alpha = if dist <= maxDist
                       then 0.0
                       else if dist <= maxDist + soft
                       then (dist - maxDist) / soft
                       else 1.0
            in max 0.0 (min 1.0 alpha)
    in map (map calculateAlpha) pixels


applyConvolution :: [[Double]] -> [[Color]] -> [[Color]]
applyConvolution kernel pixels =
    let kh = length kernel
        kw = length (head kernel)
        h = length pixels
        w = length (head pixels)
        kh2 = kh `div` 2
        kw2 = kw `div` 2
        
        getPixel y x
            | y < 0 || y >= h || x < 0 || x >= w = Color 0 0 0 255
            | otherwise = pixels !! y !! x
        
        applyToPixel y x =
            let sumR = sum [ kernel !! ky !! kx * fromIntegral (red (getPixel (y+ky-kh2) (x+kx-kw2)))
                           | ky <- [0..kh-1], kx <- [0..kw-1] ]
                sumG = sum [ kernel !! ky !! kx * fromIntegral (green (getPixel (y+ky-kh2) (x+kx-kw2)))
                           | ky <- [0..kh-1], kx <- [0..kw-1] ]
                sumB = sum [ kernel !! ky !! kx * fromIntegral (blue (getPixel (y+ky-kh2) (x+kx-kw2)))
                           | ky <- [0..kh-1], kx <- [0..kw-1] ]
                
                clamp v = max 0 (min 255 (round v))
            in Color (clamp sumR) (clamp sumG) (clamp sumB) 255
    in [ [ applyToPixel y x | x <- [0..w-1] ] | y <- [0..h-1] ]

applyInvert :: [[Color]] -> [[Color]]
applyInvert = map (map invertColor)
  where
    invertColor c = c { red = complement (red c)
                      , green = complement (green c)
                      , blue = complement (blue c) }

applyGrayscale :: [[Color]] -> [[Color]]
applyGrayscale = map (map toGray)
  where
    toGray c =
        let gray = round (0.299 * fromIntegral (red c) +
                          0.587 * fromIntegral (green c) +
                          0.114 * fromIntegral (blue c))
        in Color gray gray gray (alpha c)

applyBrightness :: Int -> [[Color]] -> [[Color]]
applyBrightness delta = map (map adjustColor)
  where
    adjustColor c =
        let clamp :: Int -> Word8
            clamp v = fromIntegral $ max 0 (min 255 v)
            r = clamp (fromIntegral (red c) + delta)
            g = clamp (fromIntegral (green c) + delta)
            b = clamp (fromIntegral (blue c) + delta)
        in Color r g b (alpha c)

applyContrast :: Double -> [[Color]] -> [[Color]]
applyContrast factor = map (map adjustColor)
  where
    adjustColor c =
        let adjust v = round (128 + factor * (fromIntegral v - 128))
            clamp v = max 0 (min 255 v)
        in Color (clamp (adjust (red c)))
                 (clamp (adjust (green c)))
                 (clamp (adjust (blue c)))
                 (alpha c)

applyThreshold :: Word8 -> [[Color]] -> [[Color]]
applyThreshold threshold = map (map toBinary)
  where
    toBinary c =
        let brightness = round (0.299 * fromIntegral (red c) +
                                0.587 * fromIntegral (green c) +
                                0.114 * fromIntegral (blue c))
            value = if brightness > fromIntegral threshold then 255 else 0
        in Color value value value (alpha c)

applyFlipHorizontal :: [[Color]] -> [[Color]]
applyFlipHorizontal = map reverse

applyFlipVertical :: [[Color]] -> [[Color]]
applyFlipVertical = reverse

applyGlitch2 :: StdGen -> [[Color]] -> [[Color]]
applyGlitch2 gen pixels =
    let h = length pixels
        w = length (head pixels)
        
        (shiftsGen, split1) = split gen
        shifts = take h (randomRs (-10, 10) shiftsGen :: [Int])
        
        (segmentsGen, _) = split split1
        numSegments = 5 + (head (randoms segmentsGen :: [Int]) `mod` 10)
        segmentHeights = randomPartition h numSegments segmentsGen
        
        applyToRow y row =
            let shift = shifts !! y
                shifted = if shift >= 0
                         then replicate shift (Color 0 0 0 255) ++ take (w - shift) row
                         else drop (-shift) row ++ replicate (-shift) (Color 0 0 0 255)
            in take w shifted  
        
        shiftedRows = zipWith applyToRow [0..] pixels
        
        segmentStarts = scanl (+) 0 segmentHeights
        segments = zipWith (\start height -> take height (drop start shiftedRows))
                          segmentStarts segmentHeights
        
        shuffledSegments = shuffle segments (mkStdGen 42)
    in concat shuffledSegments
  where
    randomPartition total parts gen
        | parts <= 1 = [total]
        | otherwise =
            let (val, newGen) = randomR (1, total - parts + 1) gen
            in val : randomPartition (total - val) (parts - 1) newGen
    
    shuffle :: [a] -> StdGen -> [a]
    shuffle [] _ = []
    shuffle xs g =
        let (idx, g') = randomR (0, length xs - 1) g
            (before, a:after) = splitAt idx xs
        in a : shuffle (before ++ after) g'

applyGlitch :: StdGen -> [[Color]] -> [[Color]]
applyGlitch gen pixels =
    let h = length pixels
        w = length (head pixels)
        numSwaps = 12
        glitchedPixels = swapBlocks pixels numSwaps gen w h
    in glitchedPixels
  where
    swapBlocks pixels 0 _ _ _ = pixels
    swapBlocks pixels n gen width height =
        let blockSize = 40
            bSize = min blockSize (min width height)
            (x1, gen2) = randomR (0, width - bSize - 1) gen
            (y1, gen3) = randomR (0, height - bSize - 1) gen2
            (x2, gen4) = randomR (0, width - bSize - 1) gen3
            (y2, gen5) = randomR (0, height - bSize - 1) gen4
            pixelsAfterSwap = swapSquareBlocks pixels x1 y1 x2 y2 bSize
        in swapBlocks pixelsAfterSwap (n - 1) gen5 width height
    
    swapSquareBlocks pixels x1 y1 x2 y2 size =
        let rows1 = [y1 .. y1 + size - 1]
            rows2 = [y2 .. y2 + size - 1]
            cols = [x1 .. x1 + size - 1]
            
            extractBlock ys xs = 
                [ [ (pixels !! y !! x) 
                  | x <- xs ] 
                | y <- ys ]
            
            block1 = extractBlock rows1 cols
            block2 = extractBlock rows2 [x2 .. x2 + size - 1]
            
            insertBlock ys xs block matrix =
                [ [ if y `elem` ys && x `elem` xs
                    then block !! (y - head ys) !! (x - head xs)
                    else matrix !! y !! x
                  | x <- [0 .. length (head matrix) - 1] ]
                | y <- [0 .. length matrix - 1] ]
            
            afterFirst = insertBlock rows1 [x2 .. x2 + size - 1] block2 pixels
            afterSecond = insertBlock rows2 [x1 .. x1 + size - 1] block1 afterFirst
        in afterSecond
        
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)


applyEffect :: Effect -> BMP -> BMP
applyEffect effect bmp@BMP{..} =
    case pixelData of
        RGB24 pixels -> bmp { pixelData = RGB24 (applyEffectToPixels effect pixels) }
        RGB32 pixels -> bmp { pixelData = RGB32 (applyEffectToPixels effect pixels) }
        _ -> bmp 
  where
    applyEffectToPixels :: Effect -> [[Color]] -> [[Color]]
    applyEffectToPixels eff pixels = case eff of
        Invert -> applyInvert pixels
        Grayscale -> applyGrayscale pixels
        Brightness delta -> applyBrightness delta pixels
        Contrast factor -> applyContrast factor pixels
        FlipHorizontal -> applyFlipHorizontal pixels
        FlipVertical -> applyFlipVertical pixels
        Sharpen -> applyConvolution createSharpenKernel pixels
        GaussianBlur radius -> 
            applyConvolution (createGaussianKernelForRadius radius) pixels
        Threshold t -> applyThreshold t pixels
        GlitchEffect gen -> applyGlitch gen pixels
        CustomKernel kernel -> applyConvolution kernel pixels

createGaussianKernelForRadius :: Int -> [[Double]]
createGaussianKernelForRadius radius =
    let size = radius * 2 + 1
        sigma = fromIntegral radius / 2.0
        
        gaussian x y = exp (-(x*x + y*y) / (2*sigma*sigma)) / (2*pi*sigma*sigma)
        
        kernel = [ [ gaussian (fromIntegral (x - radius)) (fromIntegral (y - radius))
                   | x <- [0..size-1] ]
                 | y <- [0..size-1] ]
        
        total = sum (concat kernel)
    in map (map (/total)) kernel


bmpToPixels :: BMP -> (V.Vector (Double, Double, Double, Double), Int, Int)
bmpToPixels bmp@BMP{..} =
    let width = fromIntegral $ dibWidth dibHeader
        height = fromIntegral $ dibHeight dibHeader
        pixels = case pixelData of
            RGB24 colorRows ->
                let flatColors = concat colorRows
                in V.fromList $ map colorToRGBA flatColors
            RGB32 colorRows ->
                let flatColors = concat colorRows
                in V.fromList $ map colorToRGBA flatColors
            _ -> V.empty
    in (pixels, width, height)
  where
    colorToRGBA :: Color -> (Double, Double, Double, Double)
    colorToRGBA Color{..} =
        ( fromIntegral red / 255.0
        , fromIntegral green / 255.0
        , fromIntegral blue / 255.0
        , fromIntegral alpha / 255.0
        )

pixelsToBMP :: V.Vector (Double, Double, Double, Double) -> Int -> Int -> BMP
pixelsToBMP pixels width height =
    let rows = [ [ rgbaToColor (pixels V.! (y * width + x))
                 | x <- [0..width-1] ]
               | y <- [0..height-1] ]
    in BMP
        { bmpHeader = BMPHeader
            { bmpSignature = 0x4D42
            , bmpFileSize = 0
            , bmpReserved1 = 0
            , bmpReserved2 = 0
            , bmpPixelOffset = 54
            }
        , dibHeader = DIBHeader
            { dibHeaderSize = 40
            , dibWidth = fromIntegral width
            , dibHeight = fromIntegral height
            , dibPlanes = 1
            , dibBitsPerPixel = 24
            , dibCompression = 0
            , dibImageSize = 0
            , dibXPixelsPerM = 2835
            , dibYPixelsPerM = 2835
            , dibColorsUsed = 0
            , dibColorsImportant = 0
            }
        , colorTable = Nothing
        , pixelData = RGB24 rows
        }
  where
    rgbaToColor :: (Double, Double, Double, Double) -> Color
    rgbaToColor (r, g, b, a) =
        Color (toWord8 r) (toWord8 g) (toWord8 b) (toWord8 a)
    
    toWord8 :: Double -> Word8
    toWord8 = fromIntegral . round . (* 255.0) . max 0.0 . min 1.0


blendColors :: BlendMode -> Double -> Color -> Color -> Color
blendColors mode alphaVal bgColor fgColor =
    let 
        toDouble c = (fromIntegral c :: Double) / 255.0
        fromDouble d = round (d * 255.0)
        
        brVal = toDouble (red bgColor)
        bgVal = toDouble (green bgColor)
        bbVal = toDouble (blue bgColor)
        baVal = toDouble (alpha bgColor)
        
        frVal = toDouble (red fgColor)
        fgVal = toDouble (green fgColor)
        fbVal = toDouble (blue fgColor)
        faVal = alphaVal 
        
        blendValue bgVal fgVal = 
            let result = case mode of
                    Normal -> bgVal * (1 - faVal) + fgVal * faVal
                    Multiply -> bgVal * fgVal
                    Screen -> 1 - (1 - bgVal) * (1 - fgVal)
                    Overlay -> 
                        if bgVal < 0.5
                        then 2 * bgVal * fgVal
                        else 1 - 2 * (1 - bgVal) * (1 - fgVal)
                    Additive -> min 1.0 (bgVal + fgVal)
            in max 0.0 (min 1.0 result)
        
        r = blendValue brVal frVal
        g = blendValue bgVal fgVal
        b = blendValue bbVal fbVal
        a = max baVal faVal 
        
    in Color (fromDouble r) (fromDouble g) (fromDouble b) (fromDouble a)

renderComposition :: Composition -> BMP
renderComposition Composition{..} =
    let bgColor = compBackground
        emptyRow = replicate compWidth bgColor
        background = replicate compHeight emptyRow
        
        applyLayer :: [[Color]] -> Layer -> [[Color]]
        applyLayer current Layer{..} =
            let (offsetX, offsetY) = layerPosition
                
                processedImage = foldl (flip applyEffect) layerImage layerEffects
                
                layerPixels = case pixelData processedImage of
                    RGB24 pixels -> pixels
                    RGB32 pixels -> pixels
                    _ -> []
                
                lh = length layerPixels
                lw = if lh > 0 then length (head layerPixels) else 0
                
                mask = case layerChromaKey of
                    Just chromaKey -> chromaKeyMask chromaKey layerPixels
                    Nothing -> replicate lh (replicate lw 1.0) 
                
                blendPixel y x currentColor
                    | y < offsetY || y >= offsetY + lh ||
                      x < offsetX || x >= offsetX + lw = currentColor
                    | otherwise =
                        let layerY = y - offsetY
                            layerX = x - offsetX
                            layerColor = layerPixels !! layerY !! layerX
                            maskAlpha = mask !! layerY !! layerX
                            finalAlpha = maskAlpha * layerOpacity
                        in blendColors layerBlendMode finalAlpha currentColor layerColor
                
                newPixels = [ [ blendPixel y x (current !! y !! x)
                              | x <- [0..compWidth-1] ]
                            | y <- [0..compHeight-1] ]
            in newPixels
        
        finalPixels = foldl applyLayer background (reverse compLayers)
        
        resultBMP = BMP
            { bmpHeader = BMPHeader
                { bmpSignature = 0x4D42
                , bmpFileSize = 0
                , bmpReserved1 = 0
                , bmpReserved2 = 0
                , bmpPixelOffset = 54
                }
            , dibHeader = DIBHeader
                { dibHeaderSize = 40
                , dibWidth = fromIntegral compWidth
                , dibHeight = fromIntegral compHeight
                , dibPlanes = 1
                , dibBitsPerPixel = 24
                , dibCompression = 0
                , dibImageSize = fromIntegral (compWidth * compHeight * 3)
                , dibXPixelsPerM = 2835
                , dibYPixelsPerM = 2835
                , dibColorsUsed = 0
                , dibColorsImportant = 0
                }
            , colorTable = Nothing
            , pixelData = RGB24 finalPixels
            }
    in updateFileSize resultBMP


createSimpleComposition :: Int -> Int -> Color -> [Layer] -> Composition
createSimpleComposition width height bgColor layers =
    Composition width height bgColor layers

createLayer :: BMP -> (Int, Int) -> Maybe ChromaKey -> [Effect] -> BlendMode -> Double -> Layer
createLayer img pos chromaKey effects blendMode opacity =
    Layer img chromaKey effects blendMode opacity pos

createRandomDisplacement :: Int -> Int -> StdGen -> ([[Int]], StdGen)
createRandomDisplacement width height gen =
    let (gen1, gen2) = split gen
        horizontal = [ [ fst (randomR (-maxShift, maxShift) (snd (split (gen1))))
                       | x <- [0..width-1] ]
                    | y <- [0..height-1] ]
        maxShift = min 20 (width `div` 10)
    in (horizontal, gen2)

calculateBrightness :: Color -> Double
calculateBrightness Color{..} =
    0.299 * fromIntegral red + 0.587 * fromIntegral green + 0.114 * fromIntegral blue

calculateContrast :: [[Color]] -> Double
calculateContrast pixels =
    let allPixels = concat pixels
        brightnesses = map calculateBrightness allPixels
        mean = sum brightnesses / fromIntegral (length brightnesses)
        variance = sum (map (\b -> (b - mean) ** 2) brightnesses) / fromIntegral (length brightnesses)
    in sqrt variance

exampleUsage :: IO ()
exampleUsage = do
    maybeBg <- loadBMPFile "background.bmp"
    maybeFg <- loadBMPFile "foreground.bmp"
    
    case (maybeBg, maybeFg) of
        (Just bg, Just fg) -> do
            let layer = createLayer fg (100, 100) 
                         (Just defaultGreenScreen) 
                         [] 
                         Normal 
                         1.0
            
            let width = fromIntegral $ dibWidth (dibHeader bg)
            let height = fromIntegral $ dibHeight (dibHeader bg)
            
            let comp = createSimpleComposition width height (Color 0 0 0 255) [layer]
            
            let result = renderComposition comp
            
            saveBMP "output.bmp" result
            
            putStrLn "Композиция создана и сохранена!"
        
        _ -> putStrLn "Ошибка загрузки файлов"

saveImage :: FilePath -> BMP -> IO (Either String ())
saveImage path bmp = do
    saveBMP path bmp
    return $ Right ()