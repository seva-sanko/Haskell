module Main where

import Lib
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [inputFile, outputFile] -> do
            putStrLn $ "Reading " ++ inputFile ++ "..."
            
            -- Используем readFile вместо B.readFile
            content <- readFile inputFile
            
            -- Используем parsePNM напрямую, без attoparsec
            case parsePNM content of
                Left err -> putStrLn $ "Error parsing file: " ++ err
                Right img -> do
                    putStrLn "Parsed successfully!"
                    putStrLn $ "Format: " ++ show (pnmFormat img)
                    putStrLn $ "Size: " ++ show (pnmWidth img) ++ "x" ++ show (pnmHeight img)
                    
                    putStrLn "Starting parallel processing..."
                    
                    let processedImg = processImageParallel img
                    
                    putStrLn $ "Saving to " ++ outputFile ++ "..."
                    savePNM outputFile processedImg
                    putStrLn "Done."
                    
        _ -> putStrLn "Usage: part1-exe <input.pnm> <output.pnm>"