# dc-lang
A toy imperative programming language with a tracing JIT.
FASTER THAN C!

## Requirements
* C++17 compiler
* CMake >= 3.8
* LLVM 7 (optional, you can build LLVM by setting DC_LANG_USE_SYSTEM_LLVM to OFF)

## Benchmarks
Wall-clock time, average of 10 runs.

Environment:
\
os: Linux x86\_64 4.17.2
\
cpu: Intel Core i7-8700K
\
clang: 6.0.0
\
gcc: 8.1.1
\
java: openjdk 1.8.0\_172

### Prime

| Language      | Time [s]  |
| ------------- | --------- |
| dc-lang       | 64.02     |
| dc-lang --jit | 29.06     |
| clang6 -O3    | **26.95** |
| gcc8.1 -O3    | 29.15     |
| java8         | 31.43     |

Used commands:
```
cd benchmarks
time dc-lang prime.dc
time dc-lang --jit prime.dc
clang -O3 prime.c -oprime && time ./prime
gcc -O3 prime.c -oprime && time ./prime
javac prime.java && time java prime
```

### Matrix multiplication

| Language      | Time [s]  |
| ------------- | --------- |
| dc-lang       | 105.53    |
| dc-lang --jit | 10.47     |
| clang6 -O3    | **10.08** |
| gcc8.1 -O3    | 10.27     |
| java8         | 13.71     |

Used commands:
```
cd benchmarks
python3 ../scripts/gen_matrix.py 1600 666 > matrix
time dc-lang matrix_mul.dc <matrix 1>/dev/null
time dc-lang --jit matrix_mul.dc <matrix 1>/dev/null
clang -O3 matrix_mul.c -omatrix_mul && time ./matrix_mul <matrix 1>/dev/null
gcc -O3 matrix_mul.c -omatrix_mul && time ./matrix_mul <matrix 1>/dev/null
javac matrix_mul.java && time java matrix_mul <matrix 1>/dev/null
```

### Calc

| Language      | Time [s]  |
| ------------- | --------- |
| dc-lang       | 32.64     |
| dc-lang --jit | **2.36**  |
| clang6 -O3    | 2.70      |
| gcc8.1 -O3    | 2.70      |
| java8         | 3.45      |

Used commands:
```
cd benchmarks
echo "1000000000 4" | time dc-lang calc.dc
echo "1000000000 4" | time dc-lang --jit calc.dc
clang -O3 calc.c -ocalc && echo "1000000000 4" | time ./calc
gcc -O3 calc.c -ocalc && echo "1000000000 4" | time ./calc
javac calc.java && echo "1000000000 4" | time java -Xmx12G calc
```

## License
This is free and unencumbered software released into the public domain.
For more information, see <https://unlicense.org/>.

## Acknowledgement
The design of the abstract machine and some implementation techniques were inspired by [The Programming Language Lua](https://www.lua.org/) and [The LuaJIT Project](https://luajit.org/).
