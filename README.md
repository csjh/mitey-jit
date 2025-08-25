note: This repo contains two JIT backends: the [minimal backend](https://github.com/csjh/mitey-jit/blob/main/backend/arm64/minimal.cpp) (described below), and the [baseline backend](https://github.com/csjh/mitey-jit/blob/main/backend/arm64/baseline.cpp), which is a more traditional single-pass compiler.

# The Mitey JIT

Mitey JIT is a WebAssembly JIT compiler that's meant to explore how much impact different optimizations have on the performance of WebAssembly code. Not meant for production use, but passes the WebAssembly spec tests.

Currently only expected to support Arm64 on MacOS.

The JIT is effectively an interpreter unrolled into calls to handlers for each bytecode, but this will change over time. Originally, the only instruction set-specific requirements was moving integer literals into registers, and calling a register, but this has changed to include control flow as well, and will be changed more in the future.

The current implementation uses a few tricks to force the bytecode handlers to work without the knowledge that it's being put inside generated assembly. Mainly, since all the values must stay in the same registers before and after function invocation, the `[[clang::musttail]]` attribute is used to trick the compiler into preserving them. In the general case, the handlers jump to a dummy function that "uses" the values via a dummy inline assembly block. For control flow, the handlers are given a pointer to the next instruction, which is then cast into a function pointer and jumped to. This works in Harvard architectures (most things) because the code is just a pointer somewhere in memory, so jumping to it is somewhat reasonable, and since it's forcing a tail call with `musttail`, it's guaranteed to turn into a `jmp`.

List of some of the cooler optimizations:

-   OOB Handling with `mmap` and `mprotect`
-   The above has the added benefit of base address never changing, and copy-free regrows
-   Templating common cases for generalized handlers (mainly `br`), allowing much faster execution
-   JITting `br` and other control flow instructions, allowing it to take advantage of branch prediction
-   Optimizing generation of `mov` instructions
-   Minimize parameters in calling convention, to allow for everything important to move around in registers
-   Implement custom stack using guard pages, removing need for a check on every push
-   Use `__attribute__((noinline))` and `__attribute__((preserve_most))` in cold trap paths to lessen register pressure
-   Implemented host function calls via templating, making overhead effectively just pushing and popping the parameters and result to/from stack

Planned:

-   Further (ab)use registers by using the floating point registers as general purpose registers
-   Try to speed up stack handling by using offsets instead of incrementing/decrementing in each operation
