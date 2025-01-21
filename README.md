# Maze generator and solver

This project was made in groups of 2 and can generate, solve and print mazes using backtracking in Ocaml. This project was made during my second year of university.

# Members who contributed to this project

Raphael LEONARDI
Baptiste PRAS

# How to use it ?

You first need to compile the code, do `dune build` for this. `dune clean` will remove any file linked to the compilation.

Then execute `main.exe` with valid options. Here are all the possibilities:

- ./main.exe print <name_of_a_maze>: print a given maze. Some mazes are given in `test`
- ./main.exe solve <name_of_a_maze>: solve a given maze and print the solution.
- ./main.exe random <height> <width>: generate and print a random maze of height*width size. Height and width must be strictly positive integers

With print and solve options, you can add --pretty to print a better looking version of the maze. For example, do `./main.exe solve --pretty test/maze_4x8.laby`.

Do `./main.exe --help` to be recalled these (in French).
