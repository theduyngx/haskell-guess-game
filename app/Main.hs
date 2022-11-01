{-
Author    : The Duy Nguyen
Purpose   : Test program
Copyright : (c) 2020 The University of Melbourne, modifed based on
            Peter Schachte's code.

Entry program code of the guessing game. It will ask user to input a
3-pitch chord, and then using functions in GuessGame.hs to make the
guesses.
-}

module Main (main) where

import GuessGame

-- | Guess the given target, counting and showing the guesses.
guessTest :: [Pitch] -> IO ()
guessTest target =
    case length target of
         3 -> let (guess,other) = initialGuess
              in  loop target guess other 1
         _ -> error "Invalid target input!"

-- | Given a target and guess and a guess number, continue guessing
--   until the right target is guessed.
loop :: [Pitch] -> [Pitch] -> GameState -> Int -> IO ()
loop target guess other guesses = do
    let answer = feedback target guess
    putStrLn $ "Your guess #" ++ show guesses ++ ":  " ++ show guess
    putStrLn $ "    My answer:  " ++ show answer
    case answer of
        (3,0,0) -> putStrLn $ "You got it in " ++ show guesses ++ " guesses!"
        _       -> let (guess',other') = nextGuess (guess,other) answer
                   in  loop target guess' other' (guesses+1)

-- | Prompt for a target and use guessTest to try to guess it.
main :: IO ()
main = do
    putStrLn "\nTarget chord (3 pitches separated by spaces):"
    text <- getLine
    case mapM toPitch . words $ text of
         Nothing -> error "Invalid chord!"
         Just c  -> guessTest c