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
    using reg_t = uint8_t;

    // from runtime.hpp's Signature definition
    static constexpr reg_t memptr = 0;
    static constexpr reg_t miscptr = 1;
    static constexpr reg_t stackptr = 2;
    static constexpr reg_t tmp1 = 3;
    static constexpr reg_t tmp2 = 4;

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
        constexpr reg_t x6 = 6;
        // todo: test pc-relative ldr instead (smaller, maybe more perf?)
        put_mov64(code, reinterpret_cast<uint64_t>(addr), x6);
        put<uint32_t>(code, (0b1101011000111111000000u << 10) | (x6 << 5));
    }
    static constexpr size_t max_call_size = sizeof(uint32_t) * 5;

    static void put_temp1(uint8_t *&code, uint64_t value) {
        constexpr reg_t x3 = 3;
        put_mov64(code, value, x3);
    }
    static constexpr size_t max_temp1_size = sizeof(uint32_t) * 4;

    static void put_temp2(uint8_t *&code, uint64_t value) {
        constexpr reg_t x4 = 4;
        put_mov64(code, value, x4);
    }
    static constexpr size_t max_temp2_size = sizeof(uint32_t) * 4;

    static Immediate put_br(uint8_t *&code, uint32_t arity,
                            uint32_t stack_offset) {
        constexpr reg_t x6 = 6;

        // move values
        if (stack_offset + arity) {
            for (auto m = 0; m < arity; m += sizeof(uint64_t)) {
                auto src = -arity + m;
                auto dst = stack_offset + m;
                // note: this should be handled better when src/dst
                // are higher than 1<<9
                put(code, memop(LOAD, src, x6, stackptr));
                put(code, memop(STORE, dst, x6, stackptr));
            }
            put(code, sub(false, -(stack_offset + arity), stackptr, stackptr));
        }

        auto imm = Immediate((uint32_t *)code, 19, 8, sizeof(uint32_t));
        put(code, (0b01010100u << 24) | (0 << 5) | AL); // b 0x0
        return imm;
    }

    static Immediate put_br_if(uint8_t *&code, uint32_t arity,
                               uint32_t stack_offset) {
        constexpr reg_t w8 = 8;

        put(code, 0xb85f8c48u); // ldr	w8, [x2, #-0x8]!
        auto imm = Immediate((uint32_t *)code, 19, 8, sizeof(uint32_t));
        put(code, (0b00110100u << 24) | (0 << 5) | w8); // cbz w8, 0x0
        auto dest = put_br(code, arity, stack_offset);
        imm.set(code);
        return dest;
    }

    static Immediate put_if(uint8_t *&code) {
        constexpr reg_t w8 = 8;

        put(code, 0xb85f8c48u); // ldr	w8, [x2, #-0x8]!
        auto imm = Immediate((uint32_t *)code, 19, 8, sizeof(uint32_t));
        put(code, (0b00110100u << 24) | (0 << 5) | w8); // cbz w8, 0x0
        return imm;
    }

    // else is just a br(0, 0)?

  private:
    enum Condition {
        EQ = 0b0000,
        NE = 0b0001,
        AL = 0b1110,
    };

    enum MemOpType {
        STORE = 0,
        LOAD = 1 << 22,
    };

    static void put_mov64(uint8_t *&code, uint64_t value, reg_t reg) {
        constexpr uint32_t nop = 0xd503201f;
        const uint32_t movz = 0xd2800000 | reg;

        // default to mov <reg>, #0x0 as first instruction
        std::memcpy(code, &movz, sizeof(uint32_t));
        if (!value) {
            code += sizeof(uint32_t);
        }

        bool keep = false;
        for (size_t i = 0; i < sizeof(uint32_t) && value; i++) {
            auto literal = value & 0xffff;
            value >>= 16;
            put(code, mov16(true, keep, literal, i, reg));
            keep = true;
        }
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

    static inline constexpr uint32_t mov16(bool notneg, bool keep, uint16_t imm,
                                           uint8_t shift, reg_t reg) {
        constexpr auto base = 0b10010010100000000000000000000000;
        return base | (notneg << 30) | (keep << 29) | (shift << 21) |
               (imm << 5) | reg;
    }

    static inline constexpr uint32_t sub(bool sh, uint32_t imm, reg_t dst,
                                         reg_t src) {
        constexpr auto base = 0b11010001000000000000000000000000;
        return base | (sh << 22) | (imm << 10) | (src << 5) | dst;
    }

    static inline constexpr uint32_t memop(MemOpType op, uint32_t imm, reg_t rt,
                                           reg_t rn) {
        constexpr auto base = 0b11111000000000000000000000000000;
        imm &= (1 << 9) - 1;
        return base | (op << 30) | op | (imm << 12) | (rn << 5) | rt;
    }

    template <typename T> static void put(uint8_t *&code, const T &value) {
        std::memcpy(code, &value, sizeof(T));
        code += sizeof(T);
    }
};
} // namespace mitey