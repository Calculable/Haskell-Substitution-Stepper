# The Haskell Substitution Stepper (substep)

## Quick Start
Output for `stack exec substep-exe -- --help`:
```plain
The Haskell Substitution Stepper

Usage: substep-exe (step | print | list | dump)

Available options:
  -h,--help                Show this help text

Available commands:
  step                     
  print                    
  list                     
  dump                 
```

All available commands can be run with `--help` to get more info on how to use them.

### Stepping
Step through a core program:
```shell
stack exec substep-exe -- step -p src/Source2.hs
```

### Printing
Pretty print a core program:
```shell
stack exec substep-exe -- print -p src/Source2.hs
```

### Listing
List all top level bindings in a haskell source file at the core stage:
```shell
stack exec substep-exe -- list -p src/Source2.hs
```

### Dumping
Dump various output files to `./dump` useful for debugging the substep tool:
```shell
stack exec substep-exe -- dump -p src/Source2.hs
```