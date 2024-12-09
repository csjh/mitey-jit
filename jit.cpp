#include <cstring>
#include <iostream>
#include <libkern/OSCacheControl.h>
#include <sys/mman.h>
#include <unistd.h>
#include <vector>

class JITCompiler {
    // wasm function prelude
    virtual std::vector<uint8_t> get_prelude() = 0;
    // wasm function postlude
    virtual std::vector<uint8_t> get_postlude() = 0;

    // (non-wasm?) function call
    virtual std::vector<uint8_t> call(uint64_t addr) = 0;
    virtual std::vector<uint8_t> set_temp1(uint64_t value) = 0;

  public:
    std::vector<uint8_t> generateSqrtCode() {
        uint64_t sqrtf_addr = (uint64_t)&sqrtf;

        std::vector<uint8_t> code;
        auto prelude = get_prelude(), postlude = get_postlude(),
             call_sqrtf = call(sqrtf_addr);
        code.insert(code.end(), prelude.begin(), prelude.end());
        code.insert(code.end(), call_sqrtf.begin(), call_sqrtf.end());
        code.insert(code.end(), postlude.begin(), postlude.end());

        return code;
    }
};

class ARM64JITCompiler : JITCompiler {
  private:
    const size_t PAGE_SIZE = getpagesize();

    std::vector<uint8_t> u32_to_u8(std::vector<uint32_t> u32) {
        return std::vector<uint8_t>((uint8_t *)u32.data(),
                                    (uint8_t *)(u32.data() + u32.size()));
    }

    std::vector<uint32_t> mov64(uint64_t value, uint8_t reg) {
        return {movk((value >> 0) & 0xffff, 0, reg),
                movk((value >> 16) & 0xffff, 1, reg),
                movk((value >> 32) & 0xffff, 2, reg),
                movk((value >> 48) & 0xffff, 3, reg)};
    }

    uint32_t movk(uint16_t imm, uint8_t shift, uint8_t reg) {
        return (0b111100101 << 23) | (shift << 21) | (imm << 5) | reg;
    }

    std::vector<uint8_t> get_prelude() {
        auto arm = std::vector<uint32_t>{
            0xa9bf7bfd, // stp     x29, x30, [sp, #-0x10]!
            0x910003fd  // mov     x29, sp
        };

        return u32_to_u8(arm);
    }

    std::vector<uint8_t> get_postlude() {
        auto arm = std::vector<uint32_t>{
            0xa8c17bfd, // ldp     x29, x30, [sp], #0x10
            0xd65f03c0  // ret
        };

        return u32_to_u8(arm);
    }

    std::vector<uint8_t> set_temp1(uint64_t value) {
        constexpr uint8_t x6 = 6;
        auto instructions = mov64(value, x6);
        return u32_to_u8(instructions);
    }

    std::vector<uint8_t> call(uint64_t addr) {
        constexpr uint8_t x7 = 7;
        auto instructions = mov64(addr, x7);
        instructions.push_back((0b1101011000111111000000u << 10) | (x7 << 5));
        return u32_to_u8(instructions);
    }

  public:
    float compiledSqrt(float input) {
        void *execMemory =
            mmap(nullptr, PAGE_SIZE, PROT_READ | PROT_WRITE | PROT_EXEC,
                 MAP_PRIVATE | MAP_ANONYMOUS | MAP_JIT, -1, 0);

        if (execMemory == MAP_FAILED) {
            throw std::runtime_error("Memory allocation failed");
        }

        pthread_jit_write_protect_np(false);

        std::vector<uint8_t> code = generateSqrtCode();

        memcpy(execMemory, code.data(), code.size());

        pthread_jit_write_protect_np(true);

        // flush instruction cache to ensure code is executable
        sys_icache_invalidate(execMemory, code.size());

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