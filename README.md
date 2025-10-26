This is the readme for the VOCC project.
VOCC stands for Vernon Ocaml C Compiler.


# LLVM

Output llvm code from c code :
```bash
clang -S -emit-llvm tests/easy/putchar.c -o putchar.ll -O0
```
