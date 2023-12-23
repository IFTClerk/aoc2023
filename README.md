# Advent of Code 2023
Repository for my Advent of Code 2023 solutions written in pain and suffering (otherwise known as Fortran). The first day was done in Fortran77 against my better judgement (+one bit of cheating with Fortran90 intrinsic) and the rest are done in modern Fortran.
There are certainly much better ways to solve these problems, even using Fortran. These solutions should be seen as one man's attempt to learn Fortran as supposed to a collection of optimal solutions to any of the programming challenges.

## Running
Compilation is done with ``gfortran`` and ``build.sh`` provides a shell script to compile the code. Use

    ../build.sh
    
inside the directory for each day to compile the program ``main``.
Running the script with

    ../build.sh r
    
will then automatically run the binary that's produced, and, if all goes well, copy the output of the solution into your clipboard.

To run different parts of the puzzle, go in the source code and uncomment the call to the subroutine.

## Difficulty Rating
Here is my subjective view of the difficulty of each day's puzzle, which is additionally modulated by the fact that I started without knowing much modern Fortran technology.
| Day | Puzzle | Difficulty (out of 5*) |
|:---:|---|:---:|
| 1 | string parsing and converting to numbers | *** |
| 2 | string parsing with various delimiters | ** |
| 3 | finding numbers in a 2d character array | *** |
| 4 | finding matches between two sets | ** |
| 5 | tracing number through various maps | **** |
| 6 | algebra | * |
| 7 | sorting poker (sorry, camel) cards | *** |
| 8 | moving through a graph and LCM | *** |
| 9 | extrapolating sequence | * |
| 10 | trace out loop then count interior points | **** |
| 11 | pairwise Manhattan distance with weights | * |
| 12 | nonograms ad nauseam | **** |
| 13 | find planes of reflection | * |
| 14 | rolling rocks and cycle finding | ** |
| 15 | string parsing then array manipulation | *** |
| 16 | mirror room puzzle | *** |
| 17 | pathfinding with movement restrictions | ***** |
| 18 | find area of polygon | ** |
| 19 | parse stupid instructions win stupid prizes | ***** |
| 20 | flip flop chains and counters | part 1 ***; part 2 ??? |
| 21 | stepping through an infinite checkerboard | part 1 **; part 2 **** |
| 22 | Jenga | *** |
| 23 | Longest hiking path down some slippery slopes | ***** |
