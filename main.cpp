#include "arm.hpp"
#include "runtime.hpp"
#include <iostream>

int main() {
    std::unique_ptr<JITCompiler> jit = std::make_unique<ARM64JITCompiler>();

    auto PAGE_SIZE = getpagesize();

    void *execMemory =
        mmap(nullptr, PAGE_SIZE, PROT_READ | PROT_WRITE | PROT_EXEC,
             MAP_PRIVATE | MAP_ANONYMOUS | MAP_JIT, -1, 0);

    if (execMemory == MAP_FAILED) {
        throw std::runtime_error("Memory allocation failed");
    }
    if (execMemory == nullptr) {
        throw std::runtime_error("Memory allocation failed (nullptr)");
    }

    pthread_jit_write_protect_np(false);

    std::vector<uint8_t> code;
    auto prelude = jit->get_prelude(), postlude = jit->get_postlude(),
         shove = jit->set_temp1(111),
         push = jit->call((uint64_t)&mitey::ifXXconst),
         mul = jit->call((uint64_t)&mitey::i32mul),
         add = jit->call((uint64_t)&mitey::i32add);

    code.insert(code.end(), prelude.begin(), prelude.end());
    code.insert(code.end(), add.begin(), add.end());
    code.insert(code.end(), shove.begin(), shove.end());
    code.insert(code.end(), push.begin(), push.end());
    code.insert(code.end(), mul.begin(), mul.end());
    code.insert(code.end(), shove.begin(), shove.end());
    code.insert(code.end(), push.begin(), push.end());
    code.insert(code.end(), mul.begin(), mul.end());
    code.insert(code.end(), shove.begin(), shove.end());
    code.insert(code.end(), push.begin(), push.end());
    code.insert(code.end(), add.begin(), add.end());
    code.insert(code.end(), postlude.begin(), postlude.end());

    memcpy(execMemory, code.data(), code.size());

    pthread_jit_write_protect_np(true);

    // flush instruction cache to ensure code is executable
    sys_icache_invalidate(execMemory, code.size());

    mitey::Signature *addMulMul =
        reinterpret_cast<mitey::Signature *>(execMemory);

    uint32_t i1 = 42;
    uint32_t i2 = 59;
    std::cout << "Input: " << i1 << " " << i2 << std::endl;

    mitey::WasmValue stack[16] = {i1, i2};
    addMulMul(nullptr, stack + 2, nullptr, 0, 0);
    auto jit_result = stack[0].u32;
    std::cout << "addMulMul result: " << jit_result << std::endl;

    auto std_result = (((i1 + i2) * 111) * 111) + 111;

    std::cout << "C result: " << std_result << std::endl;
    std::cout << "Results match: " << (jit_result == std_result) << std::endl;

    munmap(execMemory, PAGE_SIZE);

    return 0;
}
