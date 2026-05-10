{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)
import Control.Monad.Reader (ReaderT, runReaderT, ask, lift)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON, eitherDecodeFileStrict)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Generics (Generic)
import System.Exit (exitSuccess)

data Config = Config
    { adminToken :: Text
    , userTokens :: [Text]
    , maxAttempts :: Int
    } deriving (Show, Generic)

instance FromJSON Config

data AuthError
    = InvalidToken
    | NoAttemptsLeft
    | ForbiddenResource
    deriving (Show, Eq)


type AuthStack = ExceptT AuthError (ReaderT Config IO)


checkAccess :: Text -> Bool -> AuthStack ()
checkAccess token isAdminPanel = do
    config <- lift ask
    
    if maxAttempts config <= 0
        then do
            liftIO $ putStrLn "Попыток не осталось!"
            throwError NoAttemptsLeft
        else do
            let isValid = token `elem` userTokens config || token == adminToken config
            
            if not isValid
                then do
                    liftIO $ putStrLn $ "Неверный токен! Осталось попыток: " 
                           ++ show (maxAttempts config - 1)
                    throwError InvalidToken
                else do
                    if isAdminPanel && token /= adminToken config
                        then do
                            liftIO $ putStrLn "Токен не подходит для админ-панели"
                            throwError ForbiddenResource
                        else do
                            liftIO $ putStrLn "Проверка пройдена!"
                            return ()


loadConfig :: FilePath -> IO Config
loadConfig path = do
    result <- eitherDecodeFileStrict path
    case result of
        Left err -> do
            putStrLn $ "Ошибка загрузки config.json: " ++ err
            putStrLn "Используем конфигурацию по умолчанию"
            return Config
                { adminToken = "secret123"
                , userTokens = ["abc", "def"]
                , maxAttempts = 3
                }
        Right cfg -> return cfg

askUser :: Text -> IO Text
askUser prompt = do
    T.putStr prompt
    T.getLine

isYes :: Text -> Bool
isYes input = 
    let lower = T.toLower input
    in lower == "да" || lower == "yes" || lower == "y"


main :: IO ()
main = do
    hSetBuffering stdout NoBuffering 

    config <- loadConfig "config.json"

    authLoop config 0

authLoop :: Config -> Int -> IO ()
authLoop config failedAttempts = do
    putStrLn "\n   Новая попытка    "
  
    token <- askUser "Введите токен: "
    adminInput <- askUser "Нужна админ-панель? (да/нет): "
    let needsAdmin = isYes adminInput
    
    result <- runReaderT (runExceptT (checkAccess token needsAdmin)) config
    
    case result of
        Right () -> do
            putStrLn "\n УСПЕХ: Доступ разрешен!"
            if needsAdmin
                then putStrLn "   Вы вошли в админ-панель"
                else putStrLn "   Вы вошли как пользователь"
        
        Left error -> do
            putStrLn $ "\n ОШИБКА: " ++ show error
            handleError error config failedAttempts

handleError :: AuthError -> Config -> Int -> IO ()
handleError error config failedAttempts = case error of
    InvalidToken -> do
        let newAttempts = failedAttempts + 1
        if newAttempts >= maxAttempts config
            then do
                putStrLn "Исчерпаны все попытки! Выход."
                exitSuccess
            else do
                putStrLn $ "Попробуйте еще раз (попытка " 
                       ++ show (newAttempts + 1) 
                       ++ " из " ++ show (maxAttempts config) ++ ")"
                authLoop config newAttempts
    
    NoAttemptsLeft -> do
        putStrLn "Программа завершена"
        exitSuccess
    
    ForbiddenResource -> do
        putStrLn "У вас нет прав для админ-панели"
        putStrLn "Можете попробовать войти как обычный пользователь"
        authLoop config failedAttempts