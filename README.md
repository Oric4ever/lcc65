# lcc65

C Cross-Compiler

Here is the primary source for the C Cross-Compiler targetting the 6502 processor that is included in the OSDK (Oric Software Development Kit). I am (Fabrice) putting it here so that it eventually becomes more familiar to people willing to maintain it in the long term...

Also, in order to increase its maintenability, I will try to progressively document what my 6502 code generator does. If you are interested in it, the first thing to read is Hanson & Fraser's interfacing document (docs/INTERFAC.pdf): the lcc65 compiler is built from Hanson & Fraser's frontend, and my small 6502 code generator (6502/src/gen.c).

I've just added a -G option to the compiler, this will generate a graph output (in Graphviz dot format), so that it becomes easier to explain what the code generator does... Work is done in progressive steps :

* -G does not output code, it produces a text representation of the output of Hanson & Fraser lcc frontend, that you can feed to Graphviz dot tool. For example, the visual representation of the compilation of tests/torture_test.c file gives 
