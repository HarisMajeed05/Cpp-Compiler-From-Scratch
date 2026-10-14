# C++ Compiler From Scratch

A small compiler for a subset of C++, written in C++ to learn how compilers work. The same grammar is built in two ways: by hand, and with Bison.

## What it covers

- Lexical analysis (`lexer.cpp`)
- Recursive descent parsing and basic semantic checks (`parser.cpp`)
- Assembly generation. `prog.s` is a sample of the output.
- A small x86 assembler that writes `prog_x86.bin`
- The grammar in `grammar.md`

## Folder structure

```
lexer.cpp            lexer
parser.cpp           hand written parser and code generation
parser complete.cpp  earlier full version of the parser
grammar.md           grammar of the supported language
input.cpp            sample input program
prog.s, prog_x86.s   sample assembly output
prog.bin, prog_x86.bin   sample binary output
Bison/               same grammar written for Bison
  bison.y
  cpp_subset.y
  input.cpp
  How to run.txt
```

## Build and run

Hand written parser:

```bash
g++ -std=c++17 -O2 -o parser parser.cpp
./parser
```

Bison version (Windows):

```bash
cd Bison
win_bison -o parser.cpp bison.y
g++ parser.cpp -o parser.exe
type .\input.cpp | .\parser.exe
```
