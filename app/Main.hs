{- |
Author    : The Duy Nguyen
Purpose   : Test program
Copyright : (c) 2020 The University of Melbourne, based on
            Peter Schachte's code.
-}

-- TESTING CODE

module Main where

import Data.Maybe
import GuessGame

-- | Guess the given target, counting and showing the guesses.
guessTest :: [Pitch] -> IO ()
guessTest target = do
    case length target of
         3 -> let (guess,other) = initialGuess
              in  loop target guess other 1
         _ -> error "Invalid target input!"

-- | Given a target and guess and a guess number, continue guessing
-- until the right target is guessed.
loop :: [Pitch] -> [Pitch] -> GameState -> Int -> IO ()
loop target guess other guesses = do
    let answer = feedback target guess
    putStrLn $ "Your guess #" ++ show guesses ++ ":  " ++ show guess
    putStrLn $ "    My answer:  " ++ show answer
    if answer == (3,0,0)
        then putStrLn $ "You got it in " ++ show guesses ++ " guesses!"
        else let (guess',other') = nextGuess (guess,other) answer
             in  loop target guess' other' (guesses+1)

-- | Prompt for a target and use guessTest to try to guess it.
main :: IO ()
main = do
    putStrLn "\nTarget chord (3 pitches separated by spaces): "
    text <- getLine
    guessTest . fromJust . mapM toPitch . words $ text