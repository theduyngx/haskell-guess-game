# GuessGame
The ``GuessGame`` module for *The Game of Musician*, where the user is considered the musician
who will input a 3-Pitch chord (or simply, chord) and the program will try and make as few
guesses as possible.

A Pitch is made of 2 components - Note and Octave. A note, conventionally, includes ``C``,
``D``, ``E``, ``F``, ``G``, ``A``, and ``B``. An octave is represented by a number ``1``, ``2``,
and ``3``. The program doesn't concern flat and sharp notes, or other octaves outside 1-3 range.

For each program's guess, a *feedback* will be returned with the format ``(Int, Int, Int)``. The
first integer represents the number of Pitch matches, second is number of Note matches, and third
is number of Octave matches. It should be noted that for every Pitch match, the Pitch will not be
considered for the other 2 matches. For instance, take 2 pitches ``C1 D2 D3`` and ``D1 C1 C3``.
There is 1 Pitch match, which is ``C1``, so any match related to that won't be of consideration.
We technically have 2 Octave matches, which are ``C1`` and ``D1``, and ``D3`` and ``C3``; however
since ``C1`` isn't considered we only store 1 Octave match. Likewise with Note match. Hence, the
feedback is: ``(1,1,1)``.

Here are some other examples:<br>
Feedback between ``C1 C2 C3`` and ``C2 C1 C3`` is ``(3,0,0)``.<br>
Feedback between ``C1 D2 E3`` and ``D1 C3 E2`` is ``(0,3,3)``.<br>
Feedback between ``A1 B1 C3`` and ``A2 B1 A3`` is ``(1,1,2)``.<br>
Feedback between ``A1 C1 E2`` and ``F1 B1 A2`` is ``(0,1,3)``.

## Usage

To interact with the program, run main function in module Main:
```
ghci> main
```
or run the entire stack on the correct ``cd`` directory:
```
ghci> stack run
```
to which the program will ask for user's input (a 3-pitch chord separated by whitespace) via IO.
Example of input:
```diff
ghci> main

Target chord (3 pitches separated by spaces):
A3 B2 C2
Your guess #1:  [A1,B2,C2]
    My answer:  (2,1,0)
Your guess #2:  [C1,A1,B2]
    My answer:  (1,2,0)
Your guess #3:  [C2,A2,B2]
    My answer:  (2,1,0)
Your guess #4:  [C2,B2,A3]
    My answer:  (3,0,0)
You got it in 4 guesses!
```
