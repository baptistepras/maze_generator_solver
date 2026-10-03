# Maze Generator and Solver

Generates, solves and prints mazes in OCaml, using backtracking.

## Usage

```bash
dune build
./main.exe print test/maze_4x8.laby             # print a maze
./main.exe solve test/maze_4x8.laby             # solve it and print the solution
./main.exe solve --pretty test/maze_4x8.laby    # nicer output
./main.exe random 10 20                         # random maze of height 10 and width 20
./main.exe --help                               # help (in French)
dune clean
```

Example mazes are in `test/`.

## Authors

Raphael Leonardi and Baptiste Pras.
