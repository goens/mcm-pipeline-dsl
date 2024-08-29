# AQL : A Q(ueue) Language

This repository contains the implementation of the AQL hardware description language (HDL).

# Building

To build this repository, you need Lean4, which you can set up [here](https://leanprover.github.io/lean4/doc/quickstart.html).
With Lean4 installed you can build the project by running
```
lake update && lake build
```

After compiling, you should be able to run the executable by running
```
build/bin/aqlc
```

# Known Issues
Currently you have to run the executable from this repository's main directory. This is a known bug (#3).
If you see the message:
```
uncaught exception: unknown package 'PipelineDsl'
You might need to open 'path/to/this/repository' as a workspace in your editor
```
This means that you are running `aqlc` from the wrong directory. Run it from this repositories main directory.

# Usage
The `aqlc` command takes a single input, an `aql` file, as well as optional flags:
```
    aqlc [FLAGS] <input>
```
The output can be configured with the following flags:
```
    -h, --help                             Prints this message.
    -r, --round-trip                       Round trip the AST through the parser
                                           and pretty printer
    -s, --sanity-check                     Run sanity check on the round tripped
                                           code
    -a, --pretty-print-ast                 Pretty print the AST
    -m, --emit-murphi                      Emit output Murphi
    -D, --output-dir                       Output directory for emitting Murphi
    -M, --murphi-testing                   Run Murphi-specific tests
    -t, --transformer-testing : Array Nat  Print witnesses when exploring

```

# License

See the LICENSE file
