#include "minimal.hpp"
#include <libkern/OSCacheControl.h>
#include <sys/mman.h>
#include <unistd.h>

namespace mitey {

namespace {
enum Condition {
    EQ = 0b0000,
    NE = 0b0001,
    AL = 0b1110,
};

enum MemOpType {
    STORE = 0,
    LOAD = 1 << 22,
};

using reg_t = uint8_t;

template <typename T> void put(std::byte *&code, const T &value) {
    std::memcpy(code, &value, sizeof(T));
    code += sizeof(T);
}

constexpr uint32_t mov16(bool notneg, bool keep, uint16_t imm, uint8_t shift,
                         reg_t reg) {
    constexpr auto base = 0b10010010100000000000000000000000;
    return base | (notneg << 30) | (keep << 29) | (shift << 21) | (imm << 5) |
           reg;
}

void put_mov64(std::byte *&code, uint64_t value, reg_t reg) {
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

constexpr uint32_t sub(bool sh, uint32_t imm, reg_t dst, reg_t src) {
    constexpr auto base = 0b11010001000000000000000000000000;
    return base | (sh << 22) | (imm << 10) | (src << 5) | dst;
}

constexpr uint32_t memop(MemOpType op, uint32_t imm, reg_t rt, reg_t rn) {
    constexpr auto base = 0b11111000000000000000000000000000;
    imm &= (1 << 9) - 1;
    return base | (op << 30) | op | (imm << 12) | (rn << 5) | rt;
}

}; // namespace

void Arm64::put_prelude(std::byte *&code) {
    put(code, std::array<uint32_t, 2>{
                  0xa9bf7bfd, // stp     x29, x30, [sp, #-0x10]!
                  0x910003fd  // mov     x29, sp
              });
}

void Arm64::put_postlude(std::byte *&code) {
    put(code, std::array<uint32_t, 2>{
                  0xa8c17bfd, // ldp     x29, x30, [sp], #0x10
                  0xd65f03c0  // ret
              });
}

void Arm64::put_call(std::byte *&code, runtime::Signature *addr) {
    constexpr reg_t x6 = 6;
    // todo: test pc-relative ldr instead (smaller, maybe more perf?)
    put_mov64(code, reinterpret_cast<uint64_t>(addr), x6);
    put<uint32_t>(code, (0b1101011000111111000000u << 10) | (x6 << 5));
}

void Arm64::put_temp1(std::byte *&code, uint64_t value) {
    constexpr reg_t x3 = 3;
    put_mov64(code, value, x3);
}

void Arm64::put_temp2(std::byte *&code, uint64_t value) {
    constexpr reg_t x4 = 4;
    put_mov64(code, value, x4);
}

std::byte *Arm64::put_br(std::byte *&code, uint32_t arity,
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

    auto imm = code;
    put(code, (0b01010100u << 24) | (0 << 5) | AL); // b 0x0
    return imm;
}

std::byte *Arm64::put_br_if(std::byte *&code, uint32_t arity,
                            uint32_t stack_offset) {
    constexpr reg_t w8 = 8;

    put(code, 0xb85f8c48u); // ldr	w8, [x2, #-0x8]!
    auto imm = code;
    put(code, (0b00110100u << 24) | (0 << 5) | w8); // cbz w8, 0x0
    auto dest = put_br(code, arity, stack_offset);
    put_immediate(imm, code);
    return dest;
}

std::byte *Arm64::put_if(std::byte *&code) {
    constexpr reg_t w8 = 8;

    put(code, 0xb85f8c48u); // ldr	w8, [x2, #-0x8]!
    auto imm = code;
    put(code, (0b00110100u << 24) | (0 << 5) | w8); // cbz w8, 0x0
    return imm;
}

void Arm64::put_immediate(std::byte *base, std::byte *to) {
    constexpr auto bits = 19;
    constexpr auto offset = 8;
    constexpr auto align = sizeof(uint32_t);

    auto diff = to - base;
    auto value = diff / align;

    // sign lower int32_t to intBits_t
    auto imm = static_cast<uint32_t>(value);
    imm = imm << (32 - bits) >> (32 - bits);

    uint32_t v;
    std::memcpy(&v, base, sizeof(v));

    auto high_len = offset;
    auto low_len = 32 - high_len - bits;

    auto low = (v << (high_len + bits)) >> (high_len + bits);
    auto high = (v >> (low_len + bits)) << (low_len + bits);

    uint32_t result = high | (imm << low_len) | low;
    std::memcpy(base, &result, sizeof(result));
}

} // namespace mitey