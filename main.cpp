#include "backend/arm64.hpp"
#include "pager/mac.hpp"
#include "runtime.hpp"
#include <iostream>

int main() {
    std::unique_ptr<Target> jit = std::make_unique<Arm64>();
    std::unique_ptr<Executable> pager = std::make_unique<MacExecutable>();

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

    auto mem = pager->allocate(code.size());

    pager->write(mem, [&] { memcpy(mem.ptr, code.data(), code.size()); });

    mitey::Signature *addMulMul = reinterpret_cast<mitey::Signature *>(mem.ptr);

    uint32_t i1 = 42, i2 = 59;
    std::cout << "Input: " << i1 << " " << i2 << std::endl;

    mitey::WasmValue stack[16] = {i1, i2};
    addMulMul(nullptr, stack + 2, nullptr, 0, 0);
    auto jit_result = stack[0].u32;
    std::cout << "addMulMul result: " << jit_result << std::endl;

    auto std_result = (((i1 + i2) * 111) * 111) + 111;

    std::cout << "C result: " << std_result << std::endl;
    std::cout << "Results match: " << (jit_result == std_result) << std::endl;

    pager->deallocate(mem);

    return 0;
}
