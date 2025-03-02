#pragma once

#include "../../runtime.hpp"
#include <cstddef>
#include <cstring>

namespace mitey {
class Arm64 {
    using reg_t = uint8_t;

    // from runtime.hpp's Signature definition
    static constexpr reg_t memptr = 0;
    static constexpr reg_t miscptr = 1;
    static constexpr reg_t stackptr = 2;
    static constexpr reg_t tmp1 = 3;
    static constexpr reg_t tmp2 = 4;

  public:
    template <typename F> static void placehold(std::byte *&code, F func) {
        size_t size = 0;
        if ((void *)func == (void *)put_prelude) {
            size = max_prelude_size;
        } else if ((void *)func == (void *)put_postlude) {
            size = max_postlude_size;
        } else if ((void *)func == (void *)put_call) {
            size = max_call_size;
        } else if ((void *)func == (void *)put_temp1) {
            size = max_temp1_size;
        } else if ((void *)func == (void *)put_temp2) {
            size = max_temp2_size;
        } else {
            __builtin_unreachable();
        }

        uint32_t *nooped = reinterpret_cast<uint32_t *>(code);
        for (size_t i = 0; i < size / sizeof(uint32_t); i++) {
            nooped[i] = 0xd503201f; // nop
        }
        code += size;
    }

    static void put_prelude(std::byte *&code);
    static constexpr size_t max_prelude_size = sizeof(uint32_t) * 2;

    static void put_postlude(std::byte *&code);
    static constexpr size_t max_postlude_size = sizeof(uint32_t) * 2;

    static void put_call(std::byte *&code, runtime::Signature *addr);
    static constexpr size_t max_call_size = sizeof(uint32_t) * 5;

    static void put_temp1(std::byte *&code, uint64_t value);
    static constexpr size_t max_temp1_size = sizeof(uint32_t) * 4;

    static void put_temp2(std::byte *&code, uint64_t value);
    static constexpr size_t max_temp2_size = sizeof(uint32_t) * 4;

    static std::byte *put_br(std::byte *&code, uint32_t arity,
                             uint32_t stack_offset);
    static std::byte *put_br_if(std::byte *&code, uint32_t arity,
                                uint32_t stack_offset);
    static std::byte *put_if(std::byte *&code);
    static void put_immediate(std::byte *base, std::byte *to);
};
} // namespace mitey