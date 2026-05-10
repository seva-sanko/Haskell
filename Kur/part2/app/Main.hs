{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib hiding (exampleUsage)  
import System.IO
import System.Exit
import Control.Monad
import Data.List (intercalate)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe, isNothing)
import System.Directory (doesFileExist)
import System.Random (newStdGen)

data AppState = AppState
    { currentImage :: Maybe BMP
    , currentComposition :: Maybe Composition
    , undoStack :: [BMP]  -- cтек для отмены операций
    , redoStack :: [BMP]  -- cтек для повтора операций
    } deriving (Show)

initialState :: AppState
initialState = AppState Nothing Nothing [] []


main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn " Image Processing "
    mainLoop initialState

mainLoop :: AppState -> IO ()
mainLoop state = do
    putStrLn "\n    ГЛАВНОЕ МЕНЮ "
    putStrLn "1. Загрузить изображение"
    putStrLn "2. Показать информацию об изображении"
    putStrLn "3. Эффекты обработки"
    putStrLn "4. Хромакей"
    putStrLn "5. Работа с композицией (рирпроекция)"
    putStrLn "6. Сохранить изображение"
    putStrLn "7. Отменить/Повторить"
    putStrLn "0. Выход"
    
    putStr "Выбор: "
    choice <- getLine
    
    case choice of
        "1" -> do
            newState <- loadImageMenu state
            mainLoop newState
        "2" -> do
            showImageInfo state
            mainLoop state
        "3" -> do
            newState <- effectsMenu state
            mainLoop newState
        "4" -> do
            newState <- chromaKeyMenu state
            mainLoop newState
        "5" -> do
            newState <- compositionMenu state
            mainLoop newState
        "6" -> do
            saveImageMenu state
            mainLoop state
        "7" -> do
            newState <- undoRedoMenu state
            mainLoop newState
        "0" -> do
            putStrLn "Выход..."
            exitSuccess
        _ -> do
            putStrLn "Неверный выбор. Попробуйте снова."
            mainLoop state

loadImageMenu :: AppState -> IO AppState
loadImageMenu state = do
    putStrLn "\n--- Загрузка изображения ---"
    putStr "Введите путь к файлу: "
    filepath <- getLine
    
    exists <- doesFileExist filepath
    if not exists
        then do
            putStrLn "Файл не найден!"
            return state
        else do
            putStrLn "Загрузка..."
            maybeBmp <- loadBMPFile filepath
            
            case maybeBmp of
                Nothing -> do
                    putStrLn "Ошибка: не удалось загрузить BMP файл"
                    return state
                Just bmp -> do
                    let width = fromIntegral $ dibWidth (dibHeader bmp)
                    let height = fromIntegral $ dibHeight (dibHeader bmp)
                    let bpp = dibBitsPerPixel (dibHeader bmp)
                    
                    putStrLn $ "Успешно загружено!"
                    putStrLn $ "  Размер: " ++ show width ++ "x" ++ show height
                    putStrLn $ "  Бит на пиксель: " ++ show bpp
                    
                    let newUndo = case currentImage state of
                            Just img -> img : undoStack state
                            Nothing -> undoStack state
                    
                    return state
                        { currentImage = Just bmp
                        , undoStack = newUndo
                        , redoStack = []
                        }


showImageInfo :: AppState -> IO ()
showImageInfo state = case currentImage state of
    Nothing -> putStrLn "Нет загруженного изображения."
    Just bmp -> do
        putStrLn "\n--- Информация об изображении ---"
        putStrLn $ "Размер: " ++ show (fromIntegral $ dibWidth $ dibHeader bmp) 
                  ++ "x" ++ show (fromIntegral $ dibHeight $ dibHeader bmp)
        putStrLn $ "Бит на пиксель: " ++ show (dibBitsPerPixel $ dibHeader bmp)
        putStrLn $ "Сжатие: " ++ show (dibCompression $ dibHeader bmp)
        
        case pixelData bmp of
            RGB24 pixels -> putStrLn $ "Формат: RGB24, пикселей: " ++ show (length pixels * length (head pixels))
            RGB32 pixels -> putStrLn $ "Формат: RGB32, пикселей: " ++ show (length pixels * length (head pixels))
            _ -> putStrLn "Формат: другой"


effectsMenu :: AppState -> IO AppState
effectsMenu state = case currentImage state of
    Nothing -> do
        putStrLn "Сначала загрузите изображение!"
        return state
    Just image -> do
        putStrLn "\n--- Эффекты обработки ---"
        putStrLn "1. Инверсия"
        putStrLn "2. Яркость"
        putStrLn "3. Контраст"
        putStrLn "4. Оттенки серого"
        putStrLn "5. Пороговое преобразование"
        putStrLn "6. Усиление резкости"
        putStrLn "7. Гауссово размытие"
        putStrLn "8. Отражение по горизонтали"
        putStrLn "9. Отражение по вертикали"
        putStrLn "10. Эффект глитча"
        putStrLn "0. Назад"
        
        putStr "Выбор: "
        choice <- getLine
        
        case choice of
            "1" -> applyEffectToState state Invert
            "2" -> brightnessMenu state
            "3" -> contrastMenu state
            "4" -> applyEffectToState state Grayscale
            "5" -> thresholdMenu state
            "6" -> applyEffectToState state Sharpen
            "7" -> blurMenu state
            "8" -> applyEffectToState state FlipHorizontal
            "9" -> applyEffectToState state FlipVertical
            "10" -> glitchMenu state
            "0" -> return state
            _ -> do
                putStrLn "Неверный выбор!"
                return state

applyEffectToState :: AppState -> Effect -> IO AppState
applyEffectToState state effect = do
    case currentImage state of
        Nothing -> do
            putStrLn "Нет изображения!"
            return state
        Just img -> do
            let newImg = applyEffect effect img
            
            return state
                { currentImage = Just newImg
                , undoStack = img : undoStack state
                , redoStack = []
                }

brightnessMenu :: AppState -> IO AppState
brightnessMenu state = do
    putStr "Введите значение яркости (-255..255): "
    input <- getLine
    case readMaybe input of
        Just delta | delta >= -255 && delta <= 255 -> 
            applyEffectToState state (Brightness delta)
        _ -> do
            putStrLn "Неверное значение!"
            return state

contrastMenu :: AppState -> IO AppState
contrastMenu state = do
    putStr "Введите коэффициент контраста (0.0..3.0): "
    input <- getLine
    case readMaybe input of
        Just factor | factor >= 0.0 && factor <= 3.0 ->
            applyEffectToState state (Contrast factor)
        _ -> do
            putStrLn "Неверное значение!"
            return state

thresholdMenu :: AppState -> IO AppState
thresholdMenu state = do
    putStr "Введите порог (0..255): "
    input <- getLine
    case readMaybe input of
        Just threshold ->
            applyEffectToState state (Threshold (fromIntegral threshold))
        _ -> do
            putStrLn "Неверное значение!"
            return state

blurMenu :: AppState -> IO AppState
blurMenu state = do
    putStr "Введите радиус размытия (1..10): "
    input <- getLine
    case readMaybe input of
        Just radius | radius >= 1 && radius <= 10 ->
            applyEffectToState state (GaussianBlur radius)
        _ -> do
            putStrLn "Неверное значение!"
            return state

glitchMenu :: AppState -> IO AppState
glitchMenu state = do
    gen <- newStdGen
    applyEffectToState state (GlitchEffect gen)


chromaKeyMenu :: AppState -> IO AppState
chromaKeyMenu state = case currentImage state of
    Nothing -> do
        putStrLn "Сначала загрузите изображение!"
        return state
    Just image -> do
        putStrLn "\n--- Хромакей ---"
        putStrLn "1. Зеленый экран (предустановка)"
        putStrLn "2. Синий экран (предустановка)"
        putStrLn "3. Пользовательский цвет"
        putStrLn "0. Назад"
        
        putStr "Выбор: "
        choice <- getLine
        
        case choice of
            "1" -> createMaskFromChromaKey state defaultGreenScreen
            "2" -> createMaskFromChromaKey state defaultBlueScreen
            "3" -> customChromaKeyMenu state
            "0" -> return state
            _ -> do
                putStrLn "Неверный выбор!"
                return state

createMaskFromChromaKey :: AppState -> ChromaKey -> IO AppState
createMaskFromChromaKey state chromaKey = do
    case currentImage state of
        Nothing -> return state
        Just bmp -> do
            putStrLn "Создание маски хромакея..."
            let pixels = case pixelData bmp of
                    RGB24 pxs -> pxs
                    RGB32 pxs -> pxs
                    _ -> []

            let mask = chromaKeyMask chromaKey pixels
            
            putStrLn $ "Создана маска размером: " ++ show (length mask) ++ "x" 
                      ++ show (if null mask then 0 else length (head mask))
            
            return state

customChromaKeyMenu :: AppState -> IO AppState
customChromaKeyMenu state = do
    putStrLn "\n--- Настройка пользовательского хромакея ---"
    
    putStr "R (0-255): "
    rStr <- getLine
    putStr "G (0-255): "
    gStr <- getLine
    putStr "B (0-255): "
    bStr <- getLine
    
    putStr "Допуск R (0-255): "
    trStr <- getLine
    putStr "Допуск G (0-255): "
    tgStr <- getLine
    putStr "Допуск B (0-255): "
    tbStr <- getLine
    
    putStr "Размытие краев (0-255): "
    softStr <- getLine
    
    case (readMaybe rStr, readMaybe gStr, readMaybe bStr,
          readMaybe trStr, readMaybe tgStr, readMaybe tbStr,
          readMaybe softStr) of
        (Just r, Just g, Just b,
         Just tr, Just tg, Just tb,
         Just soft)
            | all (\x -> x >= 0 && x <= 255) [r,g,b,tr,tg,tb,soft] -> do
            
            let chromaKey = ChromaKey
                    { targetColor = Color (fromIntegral r) (fromIntegral g) (fromIntegral b) 255
                    , toleranceR = fromIntegral tr
                    , toleranceG = fromIntegral tg
                    , toleranceB = fromIntegral tb
                    , softEdge = fromIntegral soft
                    }
            
            createMaskFromChromaKey state chromaKey
            
        _ -> do
            putStrLn "Неверные значения!"
            return state

askCustomChromaKey :: IO (Maybe ChromaKey)
askCustomChromaKey = do
    putStrLn "\n--- Настройка пользовательского хромакея ---"

    putStr "Целевой цвет R (0-255): "
    rStr <- getLine
    putStr "Целевой цвет G (0-255): "
    gStr <- getLine
    putStr "Целевой цвет B (0-255): "
    bStr <- getLine

    putStr "Допуск по R (0-255): "
    trStr <- getLine
    putStr "Допуск по G (0-255): "
    tgStr <- getLine
    putStr "Допуск по B (0-255): "
    tbStr <- getLine

    putStr "Размытие краев (0-255): "
    softStr <- getLine

    case (readMaybe rStr, readMaybe gStr, readMaybe bStr,
          readMaybe trStr, readMaybe tgStr, readMaybe tbStr,
          readMaybe softStr) of
        (Just r, Just g, Just b, Just tr, Just tg, Just tb, Just soft)
            | all (\x -> x >= 0 && x <= 255) [r,g,b,tr,tg,tb,soft] -> do

            let key = ChromaKey
                    { targetColor = Color (fromIntegral r) (fromIntegral g) (fromIntegral b) 255
                    , toleranceR = fromIntegral tr
                    , toleranceG = fromIntegral tg
                    , toleranceB = fromIntegral tb
                    , softEdge = fromIntegral soft
                    }
            putStrLn "Пользовательский хромакей настроен."
            return (Just key)

        _ -> do
            putStrLn "Ошибка: введены неверные значения. Хромакей не будет применен."
            return Nothing

compositionMenu :: AppState -> IO AppState
compositionMenu state = do
    putStrLn "\n--- Работа с композицией ---"
    putStrLn "1. Создать новую композицию"
    putStrLn "2. Добавить слой"
    putStrLn "3. Показать слои"
    putStrLn "4. Рендерить композицию"
    putStrLn "0. Назад"
    
    putStr "Выбор: "
    choice <- getLine
    
    case choice of
        "1" -> createCompositionMenu state
        "2" -> addLayerMenu state
        "3" -> do
            showLayersMenu state
            return state  
        "4" -> renderCompositionMenu state
        "0" -> return state
        _ -> do
            putStrLn "Неверный выбор!"
            return state

createCompositionMenu :: AppState -> IO AppState
createCompositionMenu state = do
    putStrLn "\n--- Создание новой композиции ---"
    
    putStr "Ширина: "
    wStr <- getLine
    putStr "Высота: "
    hStr <- getLine
    
    putStr "Цвет фона R (0-255): "
    rStr <- getLine
    putStr "Цвет фона G (0-255): "
    gStr <- getLine
    putStr "Цвет фона B (0-255): "
    bStr <- getLine
    
    case (readMaybe wStr, readMaybe hStr,
          readMaybe rStr, readMaybe gStr, readMaybe bStr) of
        (Just width, Just height,
         Just r, Just g, Just b)
            | width > 0 && height > 0
            && all (\x -> x >= 0 && x <= 255) [r,g,b] -> do
            
            let bgColor = Color (fromIntegral r) (fromIntegral g) (fromIntegral b) 255
            let comp = Composition width height bgColor []
            
            putStrLn $ "Создана композиция " ++ show width ++ "x" ++ show height
            return state { currentComposition = Just comp }
            
        _ -> do
            putStrLn "Неверные параметры!"
            return state

addLayerMenu :: AppState -> IO AppState
addLayerMenu state = case currentComposition state of
    Nothing -> do
        putStrLn "Сначала создайте композицию!"
        return state
    Just comp -> do
        putStr "Введите путь к файлу для слоя: "
        path <- getLine

        exists <- doesFileExist path
        if not exists
            then do
                putStrLn "Файл не найден!"
                return state
            else do
                putStrLn "Загрузка изображения слоя..."
                maybeBmp <- loadBMPFile path 

                case maybeBmp of
                    Nothing -> do
                        putStrLn "Ошибка: не удалось загрузить файл изображения!"
                        return state
                    Just bmp -> do
                        putStr "Позиция X: "
                        xStr <- getLine
                        putStr "Позиция Y: "
                        yStr <- getLine

                        putStrLn "Режим смешивания:"
                        putStrLn "  1. Normal    2. Multiply    3. Screen"
                        putStrLn "  4. Overlay   5. Additive"
                        putStr "Ваш выбор (по умолчанию Normal): "
                        blendStr <- getLine

                        putStr "Прозрачность слоя (0.0-1.0, по умолчанию 1.0): "
                        opacityStr <- getLine

                        putStrLn "\n--- Настройка удаления фона (хромакей) для этого слоя ---"
                        putStrLn "1. Без удаления фона (слой будет прямоугольным)"
                        putStrLn "2. Удалить зеленый фон (стандартная настройка)"
                        putStrLn "3. Удалить синий фон (стандартная настройка)"
                        putStrLn "4. Задать свой цвет и допуски для удаления"
                        putStr "Ваш выбор: "
                        chromaChoice <- getLine

                        maybeChromaKey <- case chromaChoice of
                            "2" -> return (Just defaultGreenScreen)
                            "3" -> return (Just defaultBlueScreen)
                            "4" -> askCustomChromaKey 
                            _   -> return Nothing    

                        case (readMaybe xStr, readMaybe yStr) of
                            (Just x, Just y) -> do
                                let blendMode = case blendStr of
                                        "2" -> Multiply
                                        "3" -> Screen
                                        "4" -> Overlay
                                        "5" -> Additive
                                        _   -> Normal

                                let opacity = fromMaybe 1.0 (readMaybe opacityStr)

                                let newLayer = Layer
                                      { layerImage = bmp
                                      , layerChromaKey = maybeChromaKey
                                      , layerEffects = []
                                      , layerBlendMode = blendMode
                                      , layerOpacity = opacity
                                      , layerPosition = (x, y)
                                      }

                                let newComp = comp { compLayers = compLayers comp ++ [newLayer] }
                                putStrLn "\nСлой успешно добавлен в композицию!"
                                return state { currentComposition = Just newComp }

                            _ -> do
                                putStrLn "Ошибка: неверно введены координаты X или Y."
                                return state

showLayersMenu :: AppState -> IO ()
showLayersMenu state = case currentComposition state of
    Nothing -> putStrLn "Нет композиции!"
    Just comp -> do
        putStrLn "\n--- Слои композиции ---"
        putStrLn $ "Размер: " ++ show (compWidth comp) ++ "x" ++ show (compHeight comp)
        putStrLn $ "Фоновый цвет: " ++ show (compBackground comp)
        putStrLn $ "Количество слоев: " ++ show (length (compLayers comp))
        
        mapM_ (\(i, layer) -> do
            let pos = layerPosition layer
            putStrLn $ "  Слой " ++ show i ++ ": позиция " ++ show pos
            ) (zip [1..] (compLayers comp))

renderCompositionMenu :: AppState -> IO AppState
renderCompositionMenu state = case currentComposition state of
    Nothing -> do
        putStrLn "Нет композиции для рендеринга!"
        return state
    Just comp -> do
        putStrLn "Рендеринг композиции..."
        
        let result = renderComposition comp
        
        putStr "Введите имя файла для сохранения: "
        filename <- getLine
        
        saveBMP filename result
        putStrLn "Композиция сохранена!"
        
        return state { currentImage = Just result }


saveImageMenu :: AppState -> IO ()
saveImageMenu state = case currentImage state of
    Nothing -> putStrLn "Нет изображения для сохранения!"
    Just bmp -> do
        putStr "Введите путь для сохранения: "
        filepath <- getLine
        
        saveBMP filepath bmp
        putStrLn "Изображение сохранено!"


undoRedoMenu :: AppState -> IO AppState
undoRedoMenu state = do
    putStrLn "\n--- Отмена/Повтор ---"
    putStrLn "1. Отменить"
    putStrLn "2. Повторить"
    putStrLn "0. Назад"
    
    putStr "Выбор: "
    choice <- getLine
    
    case choice of
        "1" -> undoAction state
        "2" -> redoAction state
        "0" -> return state
        _ -> do
            putStrLn "Неверный выбор!"
            return state

undoAction :: AppState -> IO AppState
undoAction state = case undoStack state of
    [] -> do
        putStrLn "Нет действий для отмены!"
        return state
    (prev:rest) -> do
        let current = currentImage state
        return state
            { currentImage = Just prev
            , undoStack = rest
            , redoStack = maybe [] (: []) current ++ redoStack state
            }

redoAction :: AppState -> IO AppState
redoAction state = case redoStack state of
    [] -> do
        putStrLn "Нет действий для повтора!"
        return state
    (next:rest) -> do
        let current = currentImage state
        return state
            { currentImage = Just next
            , undoStack = maybe [] (: []) current ++ undoStack state
            , redoStack = rest
            }