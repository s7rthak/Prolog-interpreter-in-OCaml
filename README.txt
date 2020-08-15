So, this is my submission for Assignment-6 for COL226. The project has been built using 'ocamlbuild' and 'ocamlfind'. It is an easy-to-use build system manager which automatically
detects dependencies and can build the 'byte' or 'native' code without having us to worry about compilation of various files in various folders. The _tags file was needed as I 
used an external package called Batteries so that the build manager knows about it. The Makefile simply calls ocamlbuild to build the project. 
The various commands in Makefile :-
make - compile all files and run the main.byte
make build - compile all files
make run - run the already present main.byte

Now I have used the LazyLists module from the package Batteries. This is a package which has to be downloaded through opam. To install Batteries command is - 'opam install batteries'.
Opam is usually installed along with OCaml as it is its package-manager, if not it can be installed with 'apt-get install opam'. OCaml doesn't support lazy evaluation implicitly
so I used LazyLists from Batteries package. I could have used Streams which are similar to LazyLists but LazyLists support memoization which makes them quite useful. Also my 
experience in programming in scala where Streams are deprecated pushed me towards using LazyLists. Other useful information has been provided through comments in prolog.ml file.
Note :- The installation procedures have been tried on Linux-Ubuntu, other OSes may have something different.