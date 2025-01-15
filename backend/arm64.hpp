#pragma once

#include "../runtime.hpp"
#include <array>
#include <cstring>
#include <libkern/OSCacheControl.h>
#include <sys/mman.h>
#include <unistd.h>

namespace mitey {
class Arm64 {
  public:
    static constexpr std::array<uint8_t, sizeof(uint32_t) * 2> get_prelude() {
        auto arm = std::array<uint32_t, 2>{
            0xa9bf7bfd, // stp     x29, x30, [sp, #-0x10]!
            0x910003fd  // mov     x29, sp
        };

        return u32_to_u8(arm);
    }

    static constexpr std::array<uint8_t, sizeof(uint32_t) * 2> get_postlude() {
        auto arm = std::array<uint32_t, 2>{
            0xa8c17bfd, // ldp     x29, x30, [sp], #0x10
            0xd65f03c0  // ret
        };

        return u32_to_u8(arm);
    }

    static std::array<uint8_t, sizeof(uint32_t) * 5>
    call(runtime::Signature *addr) {
        constexpr uint8_t x6 = 6;
        // todo: test pc-relative ldr instead (smaller, maybe more perf?)
        auto put_addr = mov64(reinterpret_cast<uint64_t>(addr), x6);
        std::array<uint32_t, 5> instructions;
        std::copy(put_addr.begin(), put_addr.end(), instructions.begin());

        // call
        instructions.back() = (0b1101011000111111000000u << 10) | (x6 << 5);

        return u32_to_u8(instructions);
    }
    static constexpr size_t call_size = sizeof(call(nullptr));

    static constexpr std::array<uint8_t, sizeof(uint32_t) * 4>
    set_temp1(uint64_t value) {
        constexpr uint8_t x3 = 3;
        auto instructions = mov64(value, x3);
        return u32_to_u8(instructions);
    }
    static constexpr size_t temp1_size = sizeof(set_temp1(0));

    static constexpr std::array<uint8_t, sizeof(uint32_t) * 4>
    set_temp2(uint64_t value) {
        constexpr uint8_t x4 = 4;
        auto instructions = mov64(value, x4);
        return u32_to_u8(instructions);
    }
    static constexpr size_t temp2_size = sizeof(set_temp1(0));

  private:
    template <size_t N>
    static constexpr std::array<uint8_t, N * 4>
    u32_to_u8(std::array<uint32_t, N> u32) {
        return std::bit_cast<std::array<uint8_t, N * 4>>(u32);
    }

    static constexpr std::array<uint32_t, 4> mov64(uint64_t value,
                                                   uint8_t reg) {
        constexpr uint32_t nop = 0xd503201f;

        // default to mov <reg>, #0x0 as first instruction
        auto instructions =
            std::array<uint32_t, 4>{0xd2800000 | reg, nop, nop, nop};
        auto keep = false;
        for (size_t i = 0, j = 0; i < 4; i++) {
            auto literal = value & 0xffff;
            value >>= 16;
            if (literal == 0)
                continue;
            instructions[j++] = mov16(true, keep, literal, i, reg);
            keep = true;
        }
        return instructions;

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
};
} // namespace mitey