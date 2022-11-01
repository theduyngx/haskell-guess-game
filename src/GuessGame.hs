{-
Module  : GuessGame
Author  : The Duy Nguyen <theduyn@student.unimelb.edu.au>
Purpose : The Game of Musician where 3 pitches are inputted by user as
          target and the program will make guesses till it gets it right.

The Game of Musician considers user as the musician who passes a 3-pitch
chord. A pitch has note (C,D,...A,B) and octave (1,2,3), i.e. F2. The
program will try to make as few guesses as possible. It first constructs
all possible pitch combinations; then makes guesses and returns feedback
matches (number of note, octave and pitch matches). With these feedbacks,
it will make better guesses via process of elimination and retention.
-}

module GuessGame (Pitch, toPitch, feedback, GameState,
                  initialGuess, nextGuess) where

import Data.List (sort, group, sortOn, tails)

-- * Data types and types
-- ** A note data type
data Note = C | D | E | F | G | A | B
     deriving (Eq, Ord, Enum, Bounded, Show)
-- ** An octave data type
data Octave = One | Two | Three
     deriving (Eq, Ord, Enum, Bounded)
instance Show Octave where
     show One   = "1"
     show Two   = "2"
     show Three = "3"

-- ** A pitch data type, with note and octave
data Pitch = Pitch { note :: Note, octave :: Octave }
     deriving (Eq, Ord)
instance Show Pitch where
     show Pitch {note = n, octave = o} = show n ++ show o

-- ** Chord - 3-pitch combination (representing target and guess)
type Chord      = [Pitch]
-- ** Game's state - all current remainders
type GameState  = [Chord]
-- ** Feedback - respectively, number of Pitch, Note, Octave matches
type Feedback   = (Int,Int,Int)
-- ** Guess and its 'score' (expected number of remainders from guess)
type GuessScore = (Chord, Double)

-- * Functions relating to String to Pitch conversion
-- | Convert a character to a Note
toNote :: Char -> Maybe Note
toNote n
   | n == 'C'  = Just C 
   | n == 'D'  = Just D 
   | n == 'E'  = Just E
   | n == 'F'  = Just F 
   | n == 'G'  = Just G 
   | n == 'A'  = Just A
   | n == 'B'  = Just B 
   | otherwise = Nothing
-- | Convert a character to an Octave
toOctave :: Char -> Maybe Octave
toOctave o
   | o == '1'  = Just One
   | o == '2'  = Just Two
   | o == '3'  = Just Three
   | otherwise = Nothing

-- | Convert a String to Pitch. It returns Pitch in Maybe, as when the
--   String cannot be converted, the Pitch will be Nothing. Example:
--
--   >>> toPitch "D2"
--   Just D2
--   >>> toPitch "H3"
--   Nothing
toPitch :: String -> Maybe Pitch
toPitch [ns, os] =
    let  nmb = toNote ns in
    case nmb of
         Nothing -> Nothing
         Just n  -> let  omb = toOctave os in
                    case omb of
                         Nothing -> Nothing
                         Just o  -> Just Pitch { note = n, octave = o }
toPitch _ = Nothing

-- * Functions generating all 3-pitch chords (initial GameState)
-- | Get a list of all possible Pitch
allPitch :: [Note] -> [Octave] -> Chord
allPitch (n:ns) (o:os) = getChord (n:ns) o ++ allPitch (n:ns) os
    where getChord (x:xs) y = Pitch {note=x, octave=y} : getChord xs y
          getChord _ _      = []
allPitch _ _ = []

-- | Get all triplet combinations (chords) from a specified list;
--   Used to get all possible 3-pitch chords for GameState
triComb :: [a] -> [[a]]
triComb l = [[x,y,z] | (x:ys) <- tails l, (y:zs) <- tails ys, z <- zs]

-- | List of all Chords, which is the initial GameState
allChord :: GameState
allChord = triComb $ allPitch [(minBound::Note)..] [(minBound::Octave)..]

-- | Find number of Note/Octave matches; Used in feedback function.
--   Arguments: note/octave function, target, guess, and current number
--   of Note/Octave matches
match :: (Ord a) => (Pitch -> a) -> Chord -> Chord -> Int -> Int
match _ [] _ result = result
match _ _ [] result = result
match f (t:ts) (g:gs) s2
    | f t == f g = match f ts gs (s2+1)
    | otherwise  = if f t < f g then match f ts (g:gs) s2
                                else match f (t:ts) gs s2
 
-- | Return feedback of a guess relative to the target. If there's a
--   Pitch match, instead of reporting it as a match of Pitch, Note
--   and Octave, it will only report it as a Pitch match. Example:
--       feedback for A1,A2,B1 with target A1,B2,A3 is (1,2,1).
--       feedback for A1,A2,A3 with target A1,B2,C3 is (1,0,2).
--       feedback for A3,B2,C1 with target C3,A2,B1 is (0,3,3).
feedback :: Chord -> Chord -> Feedback
feedback tgt gss =
    (length tgt - length tgt',
     match note   (sortCompare note   tgt') (sortCompare note   gss') 0,
     match octave (sortCompare octave tgt') (sortCompare octave gss') 0)
           where  removeCommon xs ys = [x | x <- xs, x `notElem` ys]
                  sortCompare f t    = sortOn f t
                  tgt' = removeCommon tgt gss
                  gss' = removeCommon gss tgt

-- * Functions relating to program's guesses
-- | Get the average frequency of element occurrences in a list
getAverage :: (Ord a) => [a] -> Double
getAverage lst = sum
    [fromIntegral (x*x) / fromIntegral (sum lst') | x <- lst']
     where lst' = map length . group . sort $ lst

-- | Find a chord's average number of remainders if it were the guess.
--   Consider this number the score - the lower, the better. The score
--   is deduced from list of feedbacks from making said chord the guess.
avgRemain :: GameState -> [Feedback] -> Chord -> GuessScore
avgRemain [] fbs gs    = (gs, getAverage fbs)
avgRemain (c:cs) fb gs = avgRemain cs (fb ++ [feedback c gs]) gs

-- | Get list of all guesses from current GameState and respective scores
allGuess :: GameState -> GameState -> [GuessScore]
allGuess gmst chord = map (avgRemain chord []) gmst

-- | Find the guess that reduces the GameState the most, viz. one with
--   lowest expected remainders (with min score). Returns as tuple with
--   guess and its score (respectively, thus use snd to extract score).
minAvgRemain :: [GuessScore] -> GuessScore -> GuessScore
minAvgRemain [] finalGuess = finalGuess
minAvgRemain (p:ps) tmp    = minAvgRemain ps tmp'
    where tmp' = if snd p < snd tmp then p else tmp

-- | Pick best guess by regarding all remaining chords as a target
--   and choose one leaving smallest expected number of remainders
pickGuess :: GameState -> Chord
pickGuess chord = fst $ minAvgRemain (allGuess chord chord)
                                     ([], fromIntegral $ length allChord)

-- | Return the initial guess without any input
initialGuess :: (Chord, GameState)
initialGuess = ([a,b,c], allChord)
     where a = Pitch {note = A, octave = One}
           b = Pitch {note = B, octave = Two}
           c = Pitch {note = C, octave = Two}

-- | Make the next guesses by narrowing down the GameState and picking
--   guesses that are probabilistically optimal
nextGuess :: (Chord, GameState) -> Feedback -> (Chord, GameState)
nextGuess (gs, gmst) fb = (pickGuess chords, chords)
           where chords = filter ((== fb) . feedback gs) gmst