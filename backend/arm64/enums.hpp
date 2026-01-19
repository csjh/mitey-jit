#pragma once

#include <array>
#include <cstdint>
#include <span>

namespace mitey {
namespace arm64 {

// clang-format off
enum class ireg : uint8_t {
     x0,  x1,  x2,  x3,  x4,  x5,  x6,  x7,  x8,  x9, x10, x11, x12, x13, x14, x15,
    x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, xzr, sp = xzr
};

enum class freg : uint8_t {
    d0,  d1,  d2,  d3,  d4,  d5,  d6,  d7,  d8,  d9,  d10, d11, d12, d13, d14, d15,
    d16, d17, d18, d19, d20, d21, d22, d23, d24, d25, d26, d27, d28, d29, d30, d31,
};

constexpr auto icallee_saved = std::to_array({
    ireg::x19, ireg::x20, ireg::x21, ireg::x22, ireg::x23,
    ireg::x24, ireg::x25, ireg::x26, ireg::x27, ireg::x28});

// these are wrong, i guess it's only d8-d15 that are callee saved
// but reg_manager doesn't like gaps yet :(
constexpr auto fcallee_saved = std::to_array({
    freg::d8,  freg::d9,  freg::d10, freg::d11, freg::d12, freg::d13, freg::d14, freg::d15,
    freg::d16, freg::d17, freg::d18, freg::d19, freg::d20, freg::d21, freg::d22, freg::d23,
    freg::d24, freg::d25, freg::d26, freg::d27, freg::d28, freg::d29, freg::d30, freg::d31});

constexpr auto icaller_saved = std::to_array({
    ireg::x3,  ireg::x4,  ireg::x5,  ireg::x6,  ireg::x7,  ireg::x8,  ireg::x9,
    ireg::x10, ireg::x11, ireg::x12, ireg::x13, ireg::x14, ireg::x15, ireg::x16, ireg::x17});

constexpr auto fcaller_saved = std::to_array({
    freg::d0, freg::d1, freg::d2, freg::d3, freg::d4, freg::d5, freg::d6, freg::d7});

// 2 registers are reserved for ldp/stp in prelude/postlude
constexpr auto iargs = std::span(icaller_saved).subspan(0, icaller_saved.size() - 2);
constexpr auto fargs = std::span(fcaller_saved).subspan(0, fcaller_saved.size() - 2);

template <typename T>
constexpr std::span<const T> arg_regs = [] {
        if constexpr (std::is_same_v<T, ireg>)
            return iargs;
        else if constexpr (std::is_same_v<T, freg>)
            return fargs;
        else
            static_assert(!std::is_same_v<T, T>, "Invalid register type");
    }();

constexpr auto isafe = std::span(icaller_saved).subspan(icaller_saved.size() - 2, 2);
constexpr auto fsafe = std::span(fcaller_saved).subspan(fcaller_saved.size() - 2, 2);

template <typename T>
constexpr auto convention_safe = [] {
    if constexpr (std::is_same_v<T, ireg>) {
        return isafe;
    } else if constexpr (std::is_same_v<T, freg>) {
        return fsafe;
    } else {
        static_assert(!std::is_same_v<T, T>, "Invalid register type");
    }
}();

// todo: maybe put in callee saved? can bench
constexpr auto memreg = ireg::x0;
constexpr auto miscreg = ireg::x1;
constexpr auto stackreg = ireg::x2;

// clang-format on

enum class cond : uint8_t {
    eq = 0b0000, // Equal.
    ne = 0b0001, // Not equal.
    cs = 0b0010, // Unsigned higher or same (or carry set).
    cc = 0b0011, // Unsigned lower (or carry clear).
    mi = 0b0100, // Negative.
    pl = 0b0101, // Positive or zero.
    vs = 0b0110, // Signed overflow.
    vc = 0b0111, // No signed overflow.
    hi = 0b1000, // Unsigned higher.
    ls = 0b1001, // Unsigned lower or same.
    ge = 0b1010, // Signed greater than or equal.
    lt = 0b1011, // Signed less than.
    gt = 0b1100, // Signed greater than.
    le = 0b1101, // Signed less than or equal.
    al = 0b1110, // Always executed.
    nv = 0b1111, // Never executed.
};
inline cond invert(cond c) {
    return static_cast<cond>(static_cast<uint8_t>(c) ^ 1);
}

enum class shifttype : uint8_t {
    lsl = 0b00, // Logical shift left.
    lsr = 0b01, // Logical shift right.
    asr = 0b10, // Arithmetic shift right.
    ror = 0b11, // Rotate right.
};

enum class ftype : uint8_t {
    single = 0b00,  // Single-precision
    double_ = 0b01, // Double-precision
    big = 0b10,     // 128 bit
    half = 0b11,    // Half-precision
};

enum class rmode : uint8_t {
    top_half = 0b1,
    regular = 0b0,
    bottom_half = regular,
};

enum class memtype : uint8_t {
    b = 0b00, // Byte
    h = 0b01, // Halfword
    w = 0b10, // Word
    x = 0b11, // Doubleword
};

enum class resexttype : uint8_t {
    str = 0b00, // Store
    uns = 0b01, // Unsigned
    dse = 0b10, // Extend to doubleword
    wse = 0b11, // Extend to word
};

enum class indexttype : uint8_t {
    uxtw = 0b010,
    lsl = 0b011,
    sxtw = 0b110,
    sxtx = 0b111,
};

enum class enctype : uint8_t {
    offset = 0b10,
    preidx = 0b11,
    pstidx = 0b01,
};

} // namespace arm64
} // namespace mitey
