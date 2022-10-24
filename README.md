# Wolkenkratzer Solver

Constraint-programming inspired solver for
[the Wolkenkratzer puzzle](https://www.janko.at/Raetsel/Wolkenkratzer/).

**Restrictions**  
Currently, the solver is only capable of solving puzzles of dimension 5 (see 'Puzzle file format
for description), where every line contains the numbers `0..5`.


## Build
Requires `ghc` with the following packages available during build:
- Data.List
- Data.Map
- Data.Set
- Control.Monad

```shell
make
```

## Run
```shell
solver < input.txt
```

## Puzzle file format
The puzzle file have to be of the following form:

- First line: dimension `n`, where the puzzle consists of a (n+1) x (n+1) grid and valid
  numbers are in the range `[0,n]`
- `(n+1) * 2` lines of integers describing the 'vertical constraints' in order 'top to bottom,
  left to right'
- `(n+1) * 2` lines of integers describing the 'horizontal constraints' in order 'left to right,
  top to bottom'

### Example

Puzzle: 
```shell
  231423
  ------
2|......|2
3|......|2
2|......|2
1|......|4
4|......|1
3|......|2
  ------
  222142
```

Input file:
```shell
5
2
2
3
2
1
2
4
1
2
4
3
2
2
2
3
2
2
2
1
4
4
1
3
2
```

