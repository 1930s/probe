# [probe][]

Concurrent Workers in Haskell

## Build, test and run all the things

``` sh
# Build the project.
stack build

# Build the executable
stack build probe:exe:probe

# Run the test suite.
stack test

# Run the benchmarks.
stack bench

# Generate documentation.
stack haddock

# Run the Probe
stack exec probe -- -f ./sample/seeds.txt -n 2
```

[probe]: https://github.com/zeroed/probe
