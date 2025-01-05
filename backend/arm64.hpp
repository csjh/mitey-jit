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
        return {mov16((value >> 0) & 0xffff, 0, reg),
                mov16((value >> 16) & 0xffff, 1, reg),
                mov16((value >> 32) & 0xffff, 2, reg),
                mov16((value >> 48) & 0xffff, 3, reg)};
    }

    static constexpr uint32_t mov16(uint16_t imm, uint8_t shift, uint8_t reg) {
        return (0b111100101 << 23) | (shift << 21) | (imm << 5) | reg;
    }
};
} // namespace mitey