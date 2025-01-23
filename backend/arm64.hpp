#pragma once

#include "../runtime.hpp"
#include "./shared.hpp"
#include <array>
#include <cstring>
#include <libkern/OSCacheControl.h>
#include <sys/mman.h>
#include <unistd.h>

namespace mitey {
class Arm64 {
  public:
    template <typename F> static void placehold(uint8_t *&code, F func) {
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

    static void put_prelude(uint8_t *&code) {
        put(code, std::array<uint32_t, 2>{
                      0xa9bf7bfd, // stp     x29, x30, [sp, #-0x10]!
                      0x910003fd  // mov     x29, sp
                  });
    }
    static constexpr size_t max_prelude_size = sizeof(uint32_t) * 2;

    static void put_postlude(uint8_t *&code) {
        put(code, std::array<uint32_t, 2>{
                      0xa8c17bfd, // ldp     x29, x30, [sp], #0x10
                      0xd65f03c0  // ret
                  });
    }
    static constexpr size_t max_postlude_size = sizeof(uint32_t) * 2;

    static void put_call(uint8_t *&code, runtime::Signature *addr) {
        constexpr uint8_t x6 = 6;
        // todo: test pc-relative ldr instead (smaller, maybe more perf?)
        put_mov64(code, reinterpret_cast<uint64_t>(addr), x6);
        put<uint32_t>(code, (0b1101011000111111000000u << 10) | (x6 << 5));
    }
    static constexpr size_t max_call_size = sizeof(uint32_t) * 5;

    static void put_temp1(uint8_t *&code, uint64_t value) {
        constexpr uint8_t x3 = 3;
        put_mov64(code, value, x3);
    }
    static constexpr size_t max_temp1_size = sizeof(uint32_t) * 4;

    static void put_temp2(uint8_t *&code, uint64_t value) {
        constexpr uint8_t x4 = 4;
        put_mov64(code, value, x4);
    }
    static constexpr size_t max_temp2_size = sizeof(uint32_t) * 4;

  private:
    static void put_mov64(uint8_t *&code, uint64_t value, uint8_t reg) {
        constexpr uint32_t nop = 0xd503201f;

        // default to mov <reg>, #0x0 as first instruction
        auto instructions =
            std::array<uint32_t, 4>{0xd2800000 | reg, nop, nop, nop};
        auto keep = false;
        size_t i, j;
        for (i = 0, j = 0; i < sizeof(uint32_t); i++) {
            auto literal = value & 0xffff;
            value >>= 16;
            if (literal == 0)
                continue;
            instructions[j++] = mov16(true, keep, literal, i, reg);
            keep = true;
        }
        std::memcpy(code, instructions.data(), sizeof(instructions));
        code += sizeof(uint32_t) * (j == 0 ? 1 : j);
        return;

        /* these additions save ~3-4% in the executable, but
         * slow down compilation by 40-60ms

        uint64_t v = value;

        // decide to use movn or movz
        int8_t zeros = 0;
        for (size_t i = 0; i < 4; i++) {
            uint16_t literal = v & 0xffff;
            v >>= 16;
            if (literal == 0)
                zeros++;
            if (literal == static_cast<uint16_t>(-1))
                zeros--;
        }

        size_t i, j;
        if (zeros >= 0) {
            auto keep = false;
            for (i = 0, j = 0; i < 4; i++) {
                auto literal = value & 0xffff;
                value >>= 16;
                if (literal == 0)
                    continue;
                instructions[j++] = mov16(true, keep, literal, i, reg);
                keep = true;
            }
        } else {
            auto notneg = false;
            for (i = 0, j = 0; i < 4; i++) {
                auto literal = value & 0xffff;
                value >>= 16;
                if (literal == static_cast<uint16_t>(-1))
                    continue;
                instructions[j++] =
                    mov16(notneg, notneg, notneg ? literal : ~literal, i,
                    reg);
                notneg = true;
            }
        }
        */
    }

    static constexpr uint32_t mov16(bool notneg, bool keep, uint16_t imm,
                                    uint8_t shift, uint8_t reg) {
        return (notneg << 30) | (keep << 29) | (0b100100101 << 23) |
               (shift << 21) | (imm << 5) | reg;
    }

    template <typename T> static void put(uint8_t *&code, const T &value) {
        std::memcpy(code, &value, sizeof(T));
        code += sizeof(T);
    }
};
} // namespace mitey