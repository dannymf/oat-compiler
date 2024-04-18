[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-24ddc0f5d75046c5622901739e7c5dd533143b0c8e959d652212380cedb1ea36.svg)](https://classroom.github.com/a/o5pVU_N-)
# HW5: Dataflow Analysis and Optimization

Quick Start:

1. clone this repository using `git clone`
2. open the folder in VSCode
3. start an OCaml sandbox terminal
4. run `make test` from the command line
5. open `bin/solver.ml`

See the general toolchain and project instructions on the course web site. The
course web pages have a link to the html version of the homework instructions.


Using ``oatc``
--------------

``oatc`` acts like the clang compiler.  Given several .oat, .ll, .c, and .o
files, it will compile the .oat and .ll files to .s files (using the oat
frontend and backend) and then combine the results with the .c and .o files to
produce an executable named a.out.  You can also compile the .ll files using
clang instead of the oat backend, which can be useful for testing
purposes.


* To run the automated test harness do:

        ./oatc --test

* To compile oat files using the oat backend:

        ./oatc path/to/foo.oat

  - creates output/foo.ll  frontend ll code
  - creates output/foo.s   backend assembly code
  - creates output/foo.o   assembled object file
  - creates a.out          linked executable

 NOTE: by default the .s and .o files are created in 
 a directory called output, and the filenames are 
 chosen so that multiple runs of the compiler will
 not overwrite previous outputs.  foo.ll will be 
 compiled first to foo.s then foo_1.s, foo_2.s, etc.

* To compile oat files using the clang backend:

        ./oatc --clang path/to/foo.oat

* Useful flags:

  | Flag                            | Description                                                                                       |
  |---------------------------------|---------------------------------------------------------------------------------------------------|
  | --regalloc {none,greedy,better} | use the specified register allocator                                                              |
  | --liveness {trivial,dataflow}   | use the specified liveness analysis                                                               |
  | --print-regs                    | prints the register usage statistics for x86 code                                                 |
  | --print-oat                     | pretty prints the Oat abstract syntax to the terminal                                             |
  | --print-ll                      | echoes the ll program to the terminal                                                             |
  | --print-x86                     | echoes the resulting .s file to the terminal                                                      |
  | --interpret-ll                  | runs the ll file through the reference interpreter and outputs the results to the console         |
  | --execute-x86                   | runs the resulting a.out file natively (applies to either the oat backend or clang-compiled code) |
  | --clang                         | compiles to assembly using clang, not the oat backend                                             |
  | -v                              | generates verbose output, showing which commands are used for linking, etc.                       |
  | -op ``<dirname>``               | change the output path [DEFAULT=output]                                                           |
  | -o                              | change the generated executable's name [DEFAULT=a.out]                                            |
  | -S                              | stop after generating .s files                                                                    |
  | -c                              | stop after generating .o files                                                                    |
  | -h or --help                    | display the list of options                                                                       |


* Example uses:

Run the test case hw3programs/fact.oat using the oat backend:

          ./oatc --execute-x86 hw3programs/fact.oat bin/runtime.c 
          120--------------------------------------------------------------- Executing: a.out
          * a.out returned 0
