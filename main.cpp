#include "arm.hpp"
#include <iostream>

int main() {
    ARM64JITCompiler jit;

    float input = 16.0f;
    float jitResult = jit.compiledSqrt(input);
    float stdResult = sqrtf(input);

    std::cout << "Input: " << input << std::endl;
    std::cout << "JIT sqrt result: " << jitResult << std::endl;
    std::cout << "Standard sqrt result: " << stdResult << std::endl;
    std::cout << "Results match: " << (jitResult == stdResult) << std::endl;

    return 0;
}