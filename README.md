# Advent of Code 2021

See also [Advent of Code homepage](https://adventofcode.com/2021)

## Running

To call the main method via `sbt`, use `sbt run <input_dir> <day> [sample]
 - `<input_dir>` should be a directory containing files named `day1.txt`, `day2.txt`, ...
 - `<day>` is given as a single integer
 - optionally, `[sample]` is the number of a sample input to use (place in the input directory)

Without a sample input number, the solution for the given day `X` will be run with `<input dir>/day<X>.txt` as input. If a sample input number `S` is given, `<input dir>/day<X>-sample<S>.txt` is used instead.

Examples:
```
# Run day 1 with day1.txt
sbt "run input/2021 1"

# Run day 2 with day2-sample1.txt
sbt "run input/2021 2 1"
```
