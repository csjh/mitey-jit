#include <cstring>
#include <iostream>
#include <libkern/OSCacheControl.h>
#include <sys/mman.h>
#include <unistd.h>
#include <vector>

class ARM64JITCompiler {
  private:
    // Page size and memory protection flags
    const size_t PAGE_SIZE = getpagesize();

    uint32_t movk(uint16_t imm, uint8_t shift, uint8_t reg) {
        return (0b111100101 << 23) | (shift << 21) | (imm << 5) | reg;
    }

    // ARM64 machine code generation for sqrt function call
    std::vector<uint32_t> generateSqrtCode() {
        uint64_t sqrtf_addr = (uint64_t)&sqrtf;

        return {
            0xa9bf7bfd, // stp     x29, x30, [sp, #-0x10]!
            0x910003fd, // mov     x29, sp
            movk((sqrtf_addr >> 0) & 0xffff, 0, 16),
            movk((sqrtf_addr >> 16) & 0xffff, 1, 16),
            movk((sqrtf_addr >> 32) & 0xffff, 2, 16),
            movk((sqrtf_addr >> 48) & 0xffff, 3, 16),
            (0b1101011000111111000000u << 10) | (16 << 5), // brl x16
            0xa8c17bfd, // ldp     x29, x30, [sp], #0x10
            0xd65f03c0  // ret
        };
    }

  public:
    // Compile and execute sqrt function
    float compiledSqrt(float input) {
        void *execMemory =
            mmap(nullptr, PAGE_SIZE, PROT_READ | PROT_WRITE | PROT_EXEC,
                 MAP_PRIVATE | MAP_ANONYMOUS | MAP_JIT, -1, 0);

        if (execMemory == MAP_FAILED) {
            throw std::runtime_error("Memory allocation failed");
        }

        pthread_jit_write_protect_np(false);

        std::vector<uint32_t> code = generateSqrtCode();
        size_t length = code.size() * sizeof(uint32_t);

        memcpy(execMemory, code.data(), length);

        pthread_jit_write_protect_np(true);

        // flush instruction cache to ensure code is executable
        sys_icache_invalidate(execMemory, length);

        using SqrtFunc = float (*)(float);

        SqrtFunc jittedSqrt = reinterpret_cast<SqrtFunc>(execMemory);
        float result = jittedSqrt(input);

        munmap(execMemory, PAGE_SIZE);

        return result;
    }
};

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