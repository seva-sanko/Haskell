module Main where

import Control.Monad.Writer
import System.IO
import Data.Char

data Operation = Add | Sub | Mul | Div deriving (Eq, Show)

data Expr = Expr Int Operation Int

type Calculation = Writer [String] (Maybe Int)

parseExpr :: String -> Maybe Expr
parseExpr str = case words str of
    [xStr, "+", yStr] -> buildExpr xStr Add yStr
    [xStr, "-", yStr] -> buildExpr xStr Sub yStr  
    [xStr, "*", yStr] -> buildExpr xStr Mul yStr
    [xStr, "/", yStr] -> buildExpr xStr Div yStr
    _ -> Nothing
  where
    buildExpr xStr op yStr = 
        readMaybe xStr >>= \x ->
        readMaybe yStr >>= \y ->
        return (Expr x op y)
    
    readMaybe s = case reads s of
        [(n, "")] -> Just n
        _ -> Nothing

calculateExpr :: Expr -> Calculation
calculateExpr (Expr x op y) = 
    let result = case op of
            Add -> Just (x + y)
            Sub -> Just (x - y)
            Mul -> Just (x * y)
            Div -> if y == 0 
                   then Nothing
                   else Just (x `div` y)
        logMsg = case result of
            Just res -> ["Success: " ++ show x ++ " " ++ opSymbol op ++ " " ++ show y ++ " = " ++ show res]
            Nothing -> ["Error: division by zero in " ++ show x ++ " / " ++ show y]
    in writer (result, logMsg)
  where
    opSymbol Add = "+"
    opSymbol Sub = "-"
    opSymbol Mul = "*"
    opSymbol Div = "/"

processLine :: String -> Calculation
processLine line = 
    case parseExpr line of
        Just expr -> calculateExpr expr
        Nothing -> writer (Nothing, ["Invalid line: " ++ line])

processFile :: FilePath -> IO [String]
processFile path = 
    openFile path ReadMode >>= \handle ->
    readAllLines handle [] >>= \lines' ->
    hClose handle >>
    let calculations = map processLine lines'
        resultsWithLogs = map runWriter calculations
        -- map :: (a -> b) -> [a] -> [b] ->
        -- runWriter :: Writer w a -> (a, w) 
        -- calculations :: [Writer [String] (Maybe Int)]
        -- map runWriter calculations :: [(Maybe Int, [String])]
        allLogs = concatMap snd resultsWithLogs
        formattedLogs = zipWith formatLine [1..] (zip lines' resultsWithLogs)
    in return (allLogs ++ formattedLogs)
  where
    readAllLines :: Handle -> [String] -> IO [String]
    readAllLines handle acc =
        hIsEOF handle >>= \eof ->
        if eof
            then return (reverse acc)
            else hGetLine handle >>= \line ->
                 readAllLines handle (line : acc)
    
    formatLine n (line, (result, _)) =
        "Line " ++ show n ++ ": '" ++ line ++ "' → " ++ 
        case result of
            Just r -> "Result: " ++ show r
            Nothing -> "Invalid"

formatResult :: [String] -> String
formatResult logs = unlines logs

isPrefix :: String -> String -> Bool
isPrefix [] _ = True
isPrefix _ [] = False
isPrefix (x:xs) (y:ys) = x == y && isPrefix xs ys

main :: IO ()
main =
    putStrLn "  Mathematical Operations Calculator" >>
    putStrLn "" >>
    processFile "operations.txt" >>= \logs ->
    putStrLn "Execution Log:" >>
    putStrLn (formatResult logs) >>
    putStrLn "" >>
    putStrLn "Statistics:" >>
    putStrLn ("Successful operations: " ++ show (countPrefix "Success:" logs)) >>
    putStrLn ("Invalid lines: " ++ show (countPrefix "Invalid line:" logs)) >>
    putStrLn ("Errors: " ++ show (countPrefix "Error:" logs))
  where
    countPrefix prefix lst = length (filter (isPrefix prefix) lst)