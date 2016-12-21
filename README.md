# StockQ
A financial programming language

## Requirements
The test environment is Ubuntu 16.04 running within VirtualBox
- opam --version 1.2.2 (used to compile OCaml and manage packages)
- OCaml 4.02.3
- llvm 3.8.0

## How to run
Go to the compiler directory and run 'make' to compile the StockQ compiler.
Use ./stockq -c < *.sq > temp.ll to compile an .sq file to LLVM IR.
