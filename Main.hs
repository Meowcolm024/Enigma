module Main where

import           Data.Char                      ( toUpper )
import           System.IO
import           Text.Read                      ( readMaybe )
import           Enigma

-- IO
repCipher :: Enigma -> IO ()
repCipher e =
    putStr "> "
        >>  hFlush stdout
        >>  getLine
        >>= putStrLn
        .   doEnigma e
        .   map toUpper
        >>  repCipher e

main :: IO ()
main = do
    putStr "Enigma Machine Simulator\nseed > "
    hFlush stdout
    seed <- getLine
    case (readMaybe seed) :: Maybe Int of
        Just s  -> repCipher $ enigmaR' s
        Nothing -> main
