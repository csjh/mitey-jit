#include "backend/arm64.hpp"
#include "pager/mac.hpp"
#include "runtime.hpp"
#include <iostream>

using namespace mitey;

int main() {
    std::unique_ptr<Target> backend = std::make_unique<Arm64>();
    std::unique_ptr<Executable> exec = std::make_unique<MacExecutable>();

    std::vector<uint8_t> code;
    auto prelude = backend->get_prelude(), postlude = backend->get_postlude(),
         shove = backend->set_temp1(111),
         push = backend->call(ifXXconst),
         mul = backend->call(i32mul), add = backend->call(i32add);

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

    auto mem = exec->allocate(code.size());

    exec->write(mem, [&] { memcpy(mem.ptr, code.data(), code.size()); });

    Signature *addMulMul = reinterpret_cast<Signature *>(mem.ptr);

    uint32_t i1 = 42, i2 = 59;
    std::cout << "Input: " << i1 << " " << i2 << std::endl;

    WasmValue stack[16] = {i1, i2};
    addMulMul(nullptr, stack + 2, nullptr, 0, 0);
    auto jit_result = stack[0].u32;
    std::cout << "addMulMul result: " << jit_result << std::endl;

    auto std_result = (((i1 + i2) * 111) * 111) + 111;

    std::cout << "C result: " << std_result << std::endl;
    std::cout << "Results match: " << (jit_result == std_result) << std::endl;

    exec->deallocate(mem);

    return 0;
}
