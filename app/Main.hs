module Main where

import System.Console.ANSI
import Data.List (intersperse)
import Data.Char (isSpace)

rstrip = reverse . dropWhile isSpace . reverse

getWord :: IO String
getWord = do
    putStr "Enter the word to guess: "
    word <- getLine
    return word

getGuess :: IO Char
getGuess = do
    putStr "Enter a letter: "
    guess <- getLine
    return (guess !! 0)

displayGuesses guesses = putStrLn ("Your guesses: " ++ intersperse ' ' guesses)

displayWord word guesses = do
    let chars = map (\c -> if (c `elem` guesses) then c else '_') word
    let msg = intersperse ' ' chars
    putStrLn msg

displayLives word guesses = putStrLn ("You have " ++ (show $ getLives word guesses) ++ " lives!")
gameOver word success = do
    clearScreen
    putStr (if success then "You won! " else "You lost! ")
    putStrLn $ "The word was " ++ word

check word guesses correct lives
    | correct == (length word) = gameOver word True
    | lives < 0 = gameOver word False
    | otherwise = loop word guesses

getLives word guesses = 6 - length [c | c <- guesses, not $ c `elem` word]

loop word guesses = do
    clearScreen

    putStrLn ""
    displayWord word guesses
    putStrLn ""
    displayLives word guesses
    displayGuesses guesses
    putStrLn ""

    guess <- getGuess

    let uniqueGuesses = if guess `elem` guesses then guesses else (guess : guesses)
    let correct = length [c | c <- word, c `elem` uniqueGuesses]

    check word uniqueGuesses correct (getLives word uniqueGuesses)

main :: IO ()
main = do
    word <- getWord
    loop word []
