#include "./baseline.hpp"
#include "../../type-templates.hpp"
#include <cstring>
#include <limits>
#include <optional>
#include <ranges>
#include <tuple>
#include <variant>

namespace mitey {
namespace arm64 {

namespace {

using inst = uint32_t;
static constexpr inst noop = 0xd503201f;

template <typename T> void put(std::byte *&code, const T &val) {
    std::memcpy(code, &val, sizeof(T));
    code += sizeof(T);
}

struct LogicalImm {
    uint32_t prefix : 9;
    uint32_t N : 1;
    uint32_t immr : 6;
    uint32_t imms : 6;
    uint32_t postfix : 10;

    constexpr LogicalImm(uint32_t N, uint32_t immr, uint32_t imms)
        : prefix(0), N(N), immr(immr), imms(imms), postfix(0) {}
    constexpr LogicalImm(uint32_t v) { *this = std::bit_cast<LogicalImm>(v); }
};

static_assert(sizeof(LogicalImm) == sizeof(uint32_t));

namespace raw {

void orr(std::byte *&code, bool sf, shifttype shift, ireg rm, uint8_t shift_imm,
         ireg rn, ireg rd) {
    assert(shift_imm < (sf ? 64 : 32));

    put(code, 0b00101010000000000000000000000000 |
                  (static_cast<uint32_t>(sf) << 31) |
                  (static_cast<uint32_t>(shift) << 22) |
                  (static_cast<uint32_t>(rm) << 16) |
                  (static_cast<uint32_t>(shift_imm) << 10) |
                  (static_cast<uint32_t>(rn) << 5) |
                  (static_cast<uint32_t>(rd) << 0));
}

void orr(std::byte *&code, bool sf, LogicalImm imm, ireg rn, ireg rd) {
    put(code, 0b00110010000000000000000000000000 |
                  (static_cast<uint32_t>(sf) << 31) |
                  (static_cast<uint32_t>(imm.N) << 22) |
                  (static_cast<uint32_t>(imm.immr) << 16) |
                  (static_cast<uint32_t>(imm.imms) << 10) |
                  (static_cast<uint32_t>(rn) << 5) |
                  (static_cast<uint32_t>(rd) << 0));
}

void and_(std::byte *&code, bool sf, shifttype shift, ireg rm,
          uint8_t shift_imm, ireg rn, ireg rd) {
    assert(shift_imm < (sf ? 64 : 32));

    put(code, 0b00001010000000000000000000000000 |
                  (static_cast<uint32_t>(sf) << 31) |
                  (static_cast<uint32_t>(shift) << 22) |
                  (static_cast<uint32_t>(rm) << 16) |
                  (static_cast<uint32_t>(shift_imm) << 10) |
                  (static_cast<uint32_t>(rn) << 5) |
                  (static_cast<uint32_t>(rd) << 0));
}

void and_(std::byte *&code, bool sf, LogicalImm imm, ireg rn, ireg rd) {
    put(code, 0b00010010000000000000000000000000 |
                  (static_cast<uint32_t>(sf) << 31) |
                  (static_cast<uint32_t>(imm.N) << 22) |
                  (static_cast<uint32_t>(imm.immr) << 16) |
                  (static_cast<uint32_t>(imm.imms) << 10) |
                  (static_cast<uint32_t>(rn) << 5) |
                  (static_cast<uint32_t>(rd) << 0));
}

void eor(std::byte *&code, bool sf, shifttype shift, ireg rm, uint8_t shift_imm,
         ireg rn, ireg rd) {
    assert(shift_imm < (sf ? 64 : 32));

    put(code, 0b01001010000000000000000000000000 |
                  (static_cast<uint32_t>(sf) << 31) |
                  (static_cast<uint32_t>(shift) << 22) |
                  (static_cast<uint32_t>(rm) << 16) |
                  (static_cast<uint32_t>(shift_imm) << 10) |
                  (static_cast<uint32_t>(rn) << 5) |
                  (static_cast<uint32_t>(rd) << 0));
}

void eor(std::byte *&code, bool sf, LogicalImm imm, ireg rn, ireg rd) {
    put(code, 0b01010010000000000000000000000000 |
                  (static_cast<uint32_t>(sf) << 31) |
                  (static_cast<uint32_t>(imm.N) << 22) |
                  (static_cast<uint32_t>(imm.immr) << 16) |
                  (static_cast<uint32_t>(imm.imms) << 10) |
                  (static_cast<uint32_t>(rn) << 5) |
                  (static_cast<uint32_t>(rd) << 0));
}

void ubfm(std::byte *&code, bool sf, LogicalImm imm, ireg rn, ireg rd) {
    put(code, 0b01010011000000000000000000000000 |
                  (static_cast<uint32_t>(sf) << 31) |
                  (static_cast<uint32_t>(imm.N) << 22) |
                  (static_cast<uint32_t>(imm.immr) << 16) |
                  (static_cast<uint32_t>(imm.imms) << 10) |
                  (static_cast<uint32_t>(rn) << 5) |
                  (static_cast<uint32_t>(rd) << 0));
}

void sbfm(std::byte *&code, bool sf, LogicalImm imm, ireg rn, ireg rd) {
    put(code, 0b00010011000000000000000000000000 |
                  (static_cast<uint32_t>(sf) << 31) |
                  (static_cast<uint32_t>(imm.N) << 22) |
                  (static_cast<uint32_t>(imm.immr) << 16) |
                  (static_cast<uint32_t>(imm.imms) << 10) |
                  (static_cast<uint32_t>(rn) << 5) |
                  (static_cast<uint32_t>(rd) << 0));
}

void sxtb(std::byte *&code, bool sf, ireg rn, ireg rd) {
    sbfm(code, sf, LogicalImm(sf, 0, 7), rn, rd);
}

void sxth(std::byte *&code, bool sf, ireg rn, ireg rd) {
    sbfm(code, sf, LogicalImm(sf, 0, 15), rn, rd);
}

void sxtw(std::byte *&code, ireg rn, ireg rd) {
    sbfm(code, true, LogicalImm(true, 0, 31), rn, rd);
}

void lsl(std::byte *&code, bool sf, ireg rm, ireg rn, ireg rd) {
    put(code, 0b00011010110000000010000000000000 |
                  (static_cast<uint32_t>(sf) << 31) |
                  (static_cast<uint32_t>(rm) << 16) |
                  (static_cast<uint32_t>(rn) << 5) |
                  (static_cast<uint32_t>(rd) << 0));
}

void lsl(std::byte *&code, bool sf, uint32_t shift_imm, ireg rn, ireg rd) {
    if (shift_imm == 0) {
        lsl(code, sf, ireg::xzr, rn, rd);
        return;
    }
    auto width = sf ? 64 : 32;
    shift_imm %= width;
    auto imms = width - shift_imm - 1;
    auto immr = imms + 1;
    ubfm(code, sf, LogicalImm(sf, immr, imms), rn, rd);
}

void lsr(std::byte *&code, bool sf, uint32_t shift_imm, ireg rn, ireg rd) {
    auto width = sf ? 64 : 32;
    shift_imm %= width;
    ubfm(code, sf, LogicalImm(sf, shift_imm, 0b011111u | sf << 5), rn, rd);
}

void lsr(std::byte *&code, bool sf, ireg rm, ireg rn, ireg rd) {
    put(code, 0b00011010110000000010010000000000 |
                  (static_cast<uint32_t>(sf) << 31) |
                  (static_cast<uint32_t>(rm) << 16) |
                  (static_cast<uint32_t>(rn) << 5) |
                  (static_cast<uint32_t>(rd) << 0));
}

void asr(std::byte *&code, bool sf, uint32_t shift_imm, ireg rn, ireg rd) {
    auto width = sf ? 64 : 32;
    shift_imm %= width;
    sbfm(code, sf, LogicalImm(sf, shift_imm, 0b011111u | sf << 5), rn, rd);
}

void asr(std::byte *&code, bool sf, ireg rm, ireg rn, ireg rd) {
    put(code, 0b00011010110000000010100000000000 |
                  (static_cast<uint32_t>(sf) << 31) |
                  (static_cast<uint32_t>(rm) << 16) |
                  (static_cast<uint32_t>(rn) << 5) |
                  (static_cast<uint32_t>(rd) << 0));
}

void extr(std::byte *&code, bool sf, ireg rm, uint32_t imms, ireg rn, ireg rd) {
    auto width = sf ? 64 : 32;
    imms %= width;
    put(code, 0b00010011100000000000000000000000 |
                  (static_cast<uint32_t>(sf) << 31) |
                  (static_cast<uint32_t>(sf) << 22) |
                  (static_cast<uint32_t>(rm) << 16) |
                  (static_cast<uint32_t>(imms) << 10) |
                  (static_cast<uint32_t>(rn) << 5) |
                  (static_cast<uint32_t>(rd) << 0));
}

void ror(std::byte *&code, bool sf, uint32_t imms, ireg rs, ireg rd) {
    extr(code, sf, rs, imms, rs, rd);
}

void ror(std::byte *&code, bool sf, ireg rm, ireg rn, ireg rd) {
    put(code, 0b00011010110000000010110000000000 |
                  (static_cast<uint32_t>(sf) << 31) |
                  (static_cast<uint32_t>(rm) << 16) |
                  (static_cast<uint32_t>(rn) << 5) |
                  (static_cast<uint32_t>(rd) << 0));
}

void addsub(std::byte *&code, bool sf, bool sub, bool setflags, bool shift,
            uint16_t imm12, ireg rn, ireg rd) {
    assert(imm12 < 1 << 12);

    put(code, 0b00010001000000000000000000000000 |
                  (static_cast<uint32_t>(sf) << 31) |
                  (static_cast<uint32_t>(sub) << 30) |
                  (static_cast<uint32_t>(setflags) << 29) |
                  (static_cast<uint32_t>(shift) << 22) |
                  (static_cast<uint32_t>(imm12) << 10) |
                  (static_cast<uint32_t>(rn) << 5) |
                  (static_cast<uint32_t>(rd) << 0));
}

void addsub(std::byte *&code, bool sf, bool sub, bool setflags, shifttype shift,
            ireg rm, uint8_t shift_n, ireg rn, ireg rd) {
    assert(shift_n < (sf ? 64 : 32));

    put(code, 0b00001011000000000000000000000000 |
                  (static_cast<uint32_t>(sf) << 31) |
                  (static_cast<uint32_t>(sub) << 30) |
                  (static_cast<uint32_t>(setflags) << 29) |
                  (static_cast<uint32_t>(shift) << 22) |
                  (static_cast<uint32_t>(rm) << 16) |
                  (static_cast<uint32_t>(shift_n) << 10) |
                  (static_cast<uint32_t>(rn) << 5) |
                  (static_cast<uint32_t>(rd) << 0));
}

void add(std::byte *&code, bool sf, ireg rd, ireg rn, uint16_t imm12,
         bool shift = false) {
    addsub(code, sf, false, false, shift, imm12, rn, rd);
}

void adds(std::byte *&code, bool sf, ireg rd, ireg rn, uint16_t imm12,
          bool shift = false) {
    addsub(code, sf, false, true, shift, imm12, rn, rd);
}

void add(std::byte *&code, bool sf, ireg rd, ireg rm, ireg rn,
         shifttype shift = shifttype::lsl, uint8_t shift_n = 0) {
    addsub(code, sf, false, false, shift, rm, shift_n, rn, rd);
}

void adds(std::byte *&code, bool sf, ireg rd, ireg rm, ireg rn,
          shifttype shift = shifttype::lsl, uint8_t shift_n = 0) {
    addsub(code, sf, false, true, shift, rm, shift_n, rn, rd);
}

void sub(std::byte *&code, bool sf, ireg rd, ireg rn, uint16_t imm12,
         bool shift = false) {
    addsub(code, sf, true, false, shift, imm12, rn, rd);
}

void subs(std::byte *&code, bool sf, ireg rd, ireg rn, uint16_t imm12,
          bool shift = false) {
    addsub(code, sf, true, true, shift, imm12, rn, rd);
}

void sub(std::byte *&code, bool sf, ireg rd, ireg rn, ireg rm,
         shifttype shift = shifttype::lsl, uint8_t shift_n = 0) {
    addsub(code, sf, true, false, shift, rm, shift_n, rn, rd);
}

void neg(std::byte *&code, bool sf, ireg rn, ireg rd) {
    sub(code, sf, rd, ireg::xzr, rn);
}

void subs(std::byte *&code, bool sf, ireg rd, ireg rn, ireg rm,
          shifttype shift = shifttype::lsl, uint8_t shift_n = 0) {
    addsub(code, sf, true, true, shift, rm, shift_n, rn, rd);
}

void cmp(std::byte *&code, bool sf, ireg rn, ireg rm,
         shifttype shift = shifttype::lsl, uint8_t shift_n = 0) {
    subs(code, sf, ireg::xzr, rn, rm, shift, shift_n);
}

void cmp(std::byte *&code, bool sf, ireg rn, uint16_t imm12) {
    subs(code, sf, ireg::xzr, rn, imm12);
}

[[maybe_unused]] void cmn(std::byte *&code, bool sf, ireg rn, ireg rm,
                          shifttype shift = shifttype::lsl,
                          uint8_t shift_n = 0) {
    adds(code, sf, ireg::xzr, rn, rm, shift, shift_n);
}

void cmn(std::byte *&code, bool sf, ireg rn, uint16_t imm12) {
    adds(code, sf, ireg::xzr, rn, imm12);
}

void ccmp(std::byte *&code, bool sf, ireg rm, cond c, ireg rn, uint8_t nzcv) {
    put(code, 0b01111010010000000000000000000000 |
                  (static_cast<uint32_t>(sf) << 31) |
                  (static_cast<uint32_t>(rm) << 16) |
                  (static_cast<uint32_t>(c) << 12) |
                  (static_cast<uint32_t>(rn) << 5) |
                  (static_cast<uint32_t>(nzcv) << 0));
}

void ccmp(std::byte *&code, bool sf, uint8_t imm, cond c, ireg rn,
          uint8_t nzcv) {
    assert(imm < 1 << 5);

    put(code, 0b01111010010000000000100000000000 |
                  (static_cast<uint32_t>(sf) << 31) |
                  (static_cast<uint32_t>(imm) << 16) |
                  (static_cast<uint32_t>(c) << 12) |
                  (static_cast<uint32_t>(rn) << 5) |
                  (static_cast<uint32_t>(nzcv) << 0));
}

void madd(std::byte *&code, bool sf, ireg rm, ireg ra, ireg rn, ireg rd) {
    put(code, 0b00011011000000000000000000000000 |
                  (static_cast<uint32_t>(sf) << 31) |
                  (static_cast<uint32_t>(rm) << 16) |
                  (static_cast<uint32_t>(ra) << 10) |
                  (static_cast<uint32_t>(rn) << 5) |
                  (static_cast<uint32_t>(rd) << 0));
}

void msub(std::byte *&code, bool sf, ireg rm, ireg ra, ireg rn, ireg rd) {
    put(code, 0b00011011000000001000000000000000 |
                  (static_cast<uint32_t>(sf) << 31) |
                  (static_cast<uint32_t>(rm) << 16) |
                  (static_cast<uint32_t>(ra) << 10) |
                  (static_cast<uint32_t>(rn) << 5) |
                  (static_cast<uint32_t>(rd) << 0));
}

void mul(std::byte *&code, bool sf, ireg rm, ireg rn, ireg rd) {
    madd(code, sf, rm, ireg::xzr, rn, rd);
}

void sdiv(std::byte *&code, bool sf, ireg rm, ireg rn, ireg rd) {
    put(code, 0b00011010110000000000110000000000 |
                  (static_cast<uint32_t>(sf) << 31) |
                  (static_cast<uint32_t>(rm) << 16) |
                  (static_cast<uint32_t>(rn) << 5) |
                  (static_cast<uint32_t>(rd) << 0));
}

void udiv(std::byte *&code, bool sf, ireg rm, ireg rn, ireg rd) {
    put(code, 0b00011010110000000000100000000000 |
                  (static_cast<uint32_t>(sf) << 31) |
                  (static_cast<uint32_t>(rm) << 16) |
                  (static_cast<uint32_t>(rn) << 5) |
                  (static_cast<uint32_t>(rd) << 0));
}

void b(std::byte *&code, int32_t _imm26) {
    assert(std::abs(_imm26) < (1 << 26) * sizeof(inst));

    uint32_t imm26 = _imm26 / sizeof(inst);
    imm26 &= 0x3ffffff;

    put(code, 0b00010100000000000000000000000000 |
                  (static_cast<uint32_t>(imm26) << 0));
}

void bcond(std::byte *&code, int32_t _imm19, cond c) {
    assert(std::abs(_imm19) < (1 << 19) * sizeof(inst));

    uint32_t imm19 = _imm19 /= sizeof(inst);
    imm19 &= 0x7ffff;

    put(code, 0b01010100000000000000000000000000 |
                  (static_cast<uint32_t>(imm19) << 5) |
                  (static_cast<uint32_t>(c) << 0));
}

void br(std::byte *&code, ireg rn) {
    put(code,
        0b11010110000111110000000000000000 | (static_cast<uint32_t>(rn) << 5));
}

void blr(std::byte *&code, ireg rn) {
    put(code,
        0b11010110001111110000000000000000 | (static_cast<uint32_t>(rn) << 5));
}

void cbnz(std::byte *&code, bool sf, int32_t _imm19, ireg rt) {
    assert(std::abs(_imm19) < (1 << 19) * sizeof(inst));

    uint32_t imm19 = _imm19 /= sizeof(inst);
    imm19 &= 0x7ffff;

    put(code, 0b00110101000000000000000000000000 |
                  (static_cast<uint32_t>(sf) << 31) |
                  (static_cast<uint32_t>(imm19) << 5) |
                  (static_cast<uint32_t>(rt) << 0));
}

void cbz(std::byte *&code, bool sf, int32_t _imm19, ireg rt) {
    assert(std::abs(_imm19) < (1 << 19) * sizeof(inst));

    uint32_t imm19 = _imm19 /= sizeof(inst);
    imm19 &= 0x7ffff;

    put(code, 0b00110100000000000000000000000000 |
                  (static_cast<uint32_t>(sf) << 31) |
                  (static_cast<uint32_t>(imm19) << 5) |
                  (static_cast<uint32_t>(rt) << 0));
}

void fcmp(std::byte *&code, bool is_double, freg rn, freg rm) {
    put(code, 0b00011110001000000010000000000000 |
                  (static_cast<uint32_t>(is_double) << 22) |
                  (static_cast<uint32_t>(rm) << 16) |
                  (static_cast<uint32_t>(rn) << 5));
}

void clz(std::byte *&code, bool sf, ireg rn, ireg rd) {
    put(code, 0b01011010110000000001000000000000 |
                  (static_cast<uint32_t>(sf) << 31) |
                  (static_cast<uint32_t>(rn) << 5) |
                  (static_cast<uint32_t>(rd) << 0));
}

void rbit(std::byte *&code, bool sf, ireg rn, ireg rd) {
    put(code, 0b01011010110000000000000000000000 |
                  (static_cast<uint32_t>(sf) << 31) |
                  (static_cast<uint32_t>(rn) << 5) |
                  (static_cast<uint32_t>(rd) << 0));
}

void cnt(std::byte *&code, bool Q, freg rn, freg rd) {
    put(code, 0b00001110001000000101100000000000 |
                  (static_cast<uint32_t>(Q) << 30) |
                  (static_cast<uint32_t>(rn) << 5) |
                  (static_cast<uint32_t>(rd) << 0));
}

void addv(std::byte *&code, bool Q, freg rn, freg rd) {
    put(code, 0b00001110001100011011100000000000 |
                  (static_cast<uint32_t>(Q) << 30) |
                  (static_cast<uint32_t>(rn) << 5) |
                  (static_cast<uint32_t>(rd) << 0));
}

void csinc(std::byte *&code, bool sf, ireg rm, cond c, ireg rn, ireg rd) {
    put(code, 0b00011010100000000000010000000000 |
                  (static_cast<uint32_t>(sf) << 31) |
                  (static_cast<uint32_t>(rm) << 16) |
                  (static_cast<uint32_t>(c) << 12) |
                  (static_cast<uint32_t>(rn) << 5) |
                  (static_cast<uint32_t>(rd) << 0));
}

void cset(std::byte *&code, bool sf, cond c, ireg rd) {
    csinc(code, sf, ireg::xzr, invert(c), ireg::xzr, rd);
}

void csel(std::byte *&code, bool sf, ireg rm, cond c, ireg rn, ireg rd) {
    put(code, 0b00011010100000000000000000000000 |
                  (static_cast<uint32_t>(sf) << 31) |
                  (static_cast<uint32_t>(rm) << 16) |
                  (static_cast<uint32_t>(c) << 12) |
                  (static_cast<uint32_t>(rn) << 5) |
                  (static_cast<uint32_t>(rd) << 0));
}

void csel(std::byte *&code, bool is_double, freg rm, cond c, freg rn, freg rd) {
    put(code, 0b00011110001000000000110000000000 |
                  (static_cast<uint32_t>(is_double) << 22) |
                  (static_cast<uint32_t>(rm) << 16) |
                  (static_cast<uint32_t>(c) << 12) |
                  (static_cast<uint32_t>(rn) << 5) |
                  (static_cast<uint32_t>(rd) << 0));
}

void mov(std::byte *&code, bool sf, ireg src, ireg dst) {
    orr(code, sf, shifttype::lsl, src, 0, ireg::xzr, dst);
}

void mov(std::byte *&code, bool sf, ftype ft, ireg src, freg dst) {
    put(code, 0b00011110001001110000000000000000 |
                  (static_cast<uint32_t>(sf) << 31) |
                  (static_cast<uint32_t>(ft) << 22) |
                  (static_cast<uint32_t>(src) << 5) |
                  (static_cast<uint32_t>(dst) << 0));
}

void mov(std::byte *&code, bool sf, ftype ft, freg src, ireg dst) {
    put(code, 0b00011110001001100000000000000000 |
                  (static_cast<uint32_t>(sf) << 31) |
                  (static_cast<uint32_t>(ft) << 22) |
                  (static_cast<uint32_t>(src) << 5) |
                  (static_cast<uint32_t>(dst) << 0));
}

void mov(std::byte *&code, ftype ft, freg src, freg dst) {
    put(code, 0b00011110001000000100000000000000 |
                  (static_cast<uint32_t>(ft) << 22) |
                  (static_cast<uint32_t>(src) << 5) |
                  (static_cast<uint32_t>(dst) << 0));
}

void mov(std::byte *&code, bool sf, bool notneg, bool keep, uint8_t hw,
         uint16_t imm, ireg rd) {
    assert(hw < 4);

    put(code, 0b00010010100000000000000000000000 |
                  (static_cast<uint32_t>(sf) << 31) |
                  (static_cast<uint32_t>(notneg) << 30) |
                  (static_cast<uint32_t>(keep) << 29) |
                  (static_cast<uint32_t>(hw) << 21) |
                  (static_cast<uint32_t>(imm) << 5) |
                  (static_cast<uint8_t>(rd) << 0));
}

void adr(std::byte *&code, int32_t imm, ireg rd) {
    assert(std::abs(imm) < (1 << 21) * sizeof(inst));

    auto immlo = imm & 0b11;
    auto immhi = imm >> 2;

    put(code, 0b00010000000000000000000000000000 |
                  (static_cast<uint32_t>(immlo) << 29) |
                  (static_cast<uint32_t>(immhi) << 5) |
                  (static_cast<uint32_t>(rd) << 0));
}

void fabs(std::byte *&code, ftype ft, freg rn, freg rd) {
    put(code, 0b00011110001000001100000000000000 |
                  (static_cast<uint32_t>(ft) << 22) |
                  (static_cast<uint32_t>(rn) << 5) |
                  (static_cast<uint32_t>(rd) << 0));
}

void fneg(std::byte *&code, ftype ft, freg rn, freg rd) {
    put(code, 0b00011110001000010100000000000000 |
                  (static_cast<uint32_t>(ft) << 22) |
                  (static_cast<uint32_t>(rn) << 5) |
                  (static_cast<uint32_t>(rd) << 0));
}

void frintp(std::byte *&code, ftype ft, freg rn, freg rd) {
    put(code, 0b00011110001001001100000000000000 |
                  (static_cast<uint32_t>(ft) << 22) |
                  (static_cast<uint32_t>(rn) << 5) |
                  (static_cast<uint32_t>(rd) << 0));
}

void frintm(std::byte *&code, ftype ft, freg rn, freg rd) {
    put(code, 0b00011110001001010100000000000000 |
                  (static_cast<uint32_t>(ft) << 22) |
                  (static_cast<uint32_t>(rn) << 5) |
                  (static_cast<uint32_t>(rd) << 0));
}

void frintz(std::byte *&code, ftype ft, freg rn, freg rd) {
    put(code, 0b00011110001001010100000000000000 |
                  (static_cast<uint32_t>(ft) << 22) |
                  (static_cast<uint32_t>(rn) << 5) |
                  (static_cast<uint32_t>(rd) << 0));
}

void frinti(std::byte *&code, ftype ft, freg rn, freg rd) {
    put(code, 0b00011110001001111100000000000000 |
                  (static_cast<uint32_t>(ft) << 22) |
                  (static_cast<uint32_t>(rn) << 5) |
                  (static_cast<uint32_t>(rd) << 0));
}

void fsqrt(std::byte *&code, ftype ft, freg rn, freg rd) {
    put(code, 0b00011110001000011100000000000000 |
                  (static_cast<uint32_t>(ft) << 22) |
                  (static_cast<uint32_t>(rn) << 5) |
                  (static_cast<uint32_t>(rd) << 0));
}

void fadd(std::byte *&code, ftype ft, freg rm, freg rn, freg rd) {
    put(code, 0b00011110001000000010100000000000 |
                  (static_cast<uint32_t>(ft) << 22) |
                  (static_cast<uint32_t>(rm) << 16) |
                  (static_cast<uint32_t>(rn) << 5) |
                  (static_cast<uint32_t>(rd) << 0));
}

void fsub(std::byte *&code, ftype ft, freg rm, freg rn, freg rd) {
    put(code, 0b00011110001000000011100000000000 |
                  (static_cast<uint32_t>(ft) << 22) |
                  (static_cast<uint32_t>(rm) << 16) |
                  (static_cast<uint32_t>(rn) << 5) |
                  (static_cast<uint32_t>(rd) << 0));
}

void fmul(std::byte *&code, ftype ft, freg rm, freg rn, freg rd) {
    put(code, 0b00011110001000000000100000000000 |
                  (static_cast<uint32_t>(ft) << 22) |
                  (static_cast<uint32_t>(rm) << 16) |
                  (static_cast<uint32_t>(rn) << 5) |
                  (static_cast<uint32_t>(rd) << 0));
}

void fdiv(std::byte *&code, ftype ft, freg rm, freg rn, freg rd) {
    put(code, 0b00011110001000000001100000000000 |
                  (static_cast<uint32_t>(ft) << 22) |
                  (static_cast<uint32_t>(rm) << 16) |
                  (static_cast<uint32_t>(rn) << 5) |
                  (static_cast<uint32_t>(rd) << 0));
}

void fcvt(std::byte *&code, ftype from, ftype to, freg rn, freg rd) {
    put(code, 0b00011110001000100100000000000000 |
                  (static_cast<uint32_t>(from) << 22) |
                  (static_cast<uint32_t>(to) << 15) |
                  (static_cast<uint32_t>(rn) << 5) |
                  (static_cast<uint32_t>(rd) << 0));
}

void fcvtzs(std::byte *&code, bool sf, ftype ft, freg rn, ireg rd) {
    put(code, 0b00011110001110000000000000000000 |
                  (static_cast<uint32_t>(sf) << 31) |
                  (static_cast<uint32_t>(ft) << 22) |
                  (static_cast<uint32_t>(rn) << 5) |
                  (static_cast<uint32_t>(rd) << 0));
}

void fcvtzu(std::byte *&code, bool sf, ftype ft, freg rn, ireg rd) {
    put(code, 0b00011110001110010000000000000000 |
                  (static_cast<uint32_t>(sf) << 31) |
                  (static_cast<uint32_t>(ft) << 22) |
                  (static_cast<uint32_t>(rn) << 5) |
                  (static_cast<uint32_t>(rd) << 0));
}

void scvtf(std::byte *&code, bool sf, ftype ft, ireg rn, freg rd) {
    put(code, 0b00011110001000100000000000000000 |
                  (static_cast<uint32_t>(sf) << 31) |
                  (static_cast<uint32_t>(ft) << 22) |
                  (static_cast<uint32_t>(rn) << 5) |
                  (static_cast<uint32_t>(rd) << 0));
}

void ucvtf(std::byte *&code, bool sf, ftype ft, ireg rn, freg rd) {
    put(code, 0b00011110001000110000000000000000 |
                  (static_cast<uint32_t>(sf) << 31) |
                  (static_cast<uint32_t>(ft) << 22) |
                  (static_cast<uint32_t>(rn) << 5) |
                  (static_cast<uint32_t>(rd) << 0));
}

void fmin(std::byte *&code, ftype ft, freg rm, freg rn, freg rd) {
    put(code, 0b00011110001000000101100000000000 |
                  (static_cast<uint32_t>(ft) << 22) |
                  (static_cast<uint32_t>(rm) << 16) |
                  (static_cast<uint32_t>(rn) << 5) |
                  (static_cast<uint32_t>(rd) << 0));
}

void fmax(std::byte *&code, ftype ft, freg rm, freg rn, freg rd) {
    put(code, 0b00011110001000000100100000000000 |
                  (static_cast<uint32_t>(ft) << 22) |
                  (static_cast<uint32_t>(rm) << 16) |
                  (static_cast<uint32_t>(rn) << 5) |
                  (static_cast<uint32_t>(rd) << 0));
}

void load(std::byte *&code, memtype ty, resexttype resext, indexttype indext,
          bool S, ireg rm, ireg rn, ireg rt) {
    put(code, 0b00111000001000000000100000000000 |
                  (static_cast<uint32_t>(ty) << 30) |
                  (static_cast<uint32_t>(resext) << 22) |
                  (static_cast<uint32_t>(rm) << 16) |
                  (static_cast<uint32_t>(indext) << 13) |
                  (static_cast<uint32_t>(S) << 12) |
                  (static_cast<uint32_t>(rn) << 5) |
                  (static_cast<uint32_t>(rt) << 0));
}

void load(std::byte *&code, memtype ty, resexttype resext, indexttype indext,
          bool S, ireg rm, ireg rn, freg rt) {
    put(code, 0b00111100001000000000100000000000 |
                  (static_cast<uint32_t>(ty) << 30) |
                  (static_cast<uint32_t>(resext) << 22) |
                  (static_cast<uint32_t>(rm) << 16) |
                  (static_cast<uint32_t>(indext) << 13) |
                  (static_cast<uint32_t>(S) << 12) |
                  (static_cast<uint32_t>(rn) << 5) |
                  (static_cast<uint32_t>(rt) << 0));
}

void ldp(std::byte *&code, bool sf, enctype enc, int16_t imm, ireg rt2, ireg rn,
         ireg rt) {
    assert(imm % 8 == 0);
    assert(imm >= -128 && imm <= 127);

    auto width = sf ? sizeof(uint64_t) : sizeof(uint32_t);
    imm /= width;
    auto imm7 = static_cast<uint8_t>(imm) & 0x7f;
    put(code, 0b00101000010000000000000000000000 |
                  (static_cast<uint32_t>(sf) << 31) |
                  (static_cast<uint32_t>(enc) << 23) |
                  (static_cast<uint32_t>(imm7) << 15) |
                  (static_cast<uint32_t>(rt2) << 10) |
                  (static_cast<uint32_t>(rn) << 5) |
                  (static_cast<uint32_t>(rt) << 0));
}

void stp(std::byte *&code, bool sf, enctype enc, int16_t imm, ireg rt2, ireg rn,
         ireg rt) {
    assert(imm % 8 == 0);
    assert(imm >= -128 && imm <= 127);

    auto width = sf ? sizeof(uint64_t) : sizeof(uint32_t);
    imm /= width;
    auto imm7 = static_cast<uint8_t>(imm) & 0x7f;
    put(code, 0b00101000000000000000000000000000 |
                  (static_cast<uint32_t>(sf) << 31) |
                  (static_cast<uint32_t>(enc) << 23) |
                  (static_cast<uint32_t>(imm7) << 15) |
                  (static_cast<uint32_t>(rt2) << 10) |
                  (static_cast<uint32_t>(rn) << 5) |
                  (static_cast<uint32_t>(rt) << 0));
}

void ldpsw(std::byte *&code, int8_t imm, ireg rt2, ireg rn, ireg rt) {
    assert(imm % 8 == 0);
    assert(imm >= -128 && imm <= 127);

    imm /= sizeof(uint32_t);
    auto imm7 = static_cast<uint8_t>(imm) & 0x7f;
    put(code, 0b01101001010000000000000000000000 |
                  (static_cast<uint32_t>(imm7) << 15) |
                  (static_cast<uint32_t>(rt2) << 10) |
                  (static_cast<uint32_t>(rn) << 5) |
                  (static_cast<uint32_t>(rt) << 0));
}

void str(std::byte *&code, bool sf, uint32_t offset, ireg rn, ireg rt) {
    auto width = sf ? sizeof(uint64_t) : sizeof(uint32_t);

    assert(offset % width == 0);
    assert(offset < (1 << 12) * width);

    offset /= width;
    put(code, 0b10111001000000000000000000000000 |
                  (static_cast<uint32_t>(sf) << 30) |
                  (static_cast<uint32_t>(offset) << 10) |
                  (static_cast<uint32_t>(rn) << 5) |
                  (static_cast<uint32_t>(rt) << 0));
}

void str(std::byte *&code, bool sf, uint32_t offset, ireg rn, freg rt) {
    auto width = sf ? sizeof(uint64_t) : sizeof(uint32_t);

    assert(offset % width == 0);
    assert(offset < (1 << 12) * width);

    offset /= width;
    put(code, 0b10111101000000000000000000000000 |
                  (static_cast<uint32_t>(sf) << 30) |
                  (static_cast<uint32_t>(offset) << 10) |
                  (static_cast<uint32_t>(rn) << 5) |
                  (static_cast<uint32_t>(rt) << 0));
}

template <typename RegType>
void str(std::byte *&code, bool sf, ireg rn, RegType rt) {
    str(code, sf, 0, rn, rt);
}

void ldr(std::byte *&code, bool sf, uint32_t offset, ireg rn, ireg rt) {
    auto width = sf ? sizeof(uint64_t) : sizeof(uint32_t);

    assert(offset % width == 0);
    assert(offset < (1 << 12) * width);

    offset /= width;
    put(code, 0b10111001010000000000000000000000 |
                  (static_cast<uint32_t>(sf) << 30) |
                  (static_cast<uint32_t>(offset) << 10) |
                  (static_cast<uint32_t>(rn) << 5) |
                  (static_cast<uint32_t>(rt) << 0));
}

void ldr(std::byte *&code, bool sf, uint32_t offset, ireg rn, freg rt) {
    auto width = sf ? sizeof(uint64_t) : sizeof(uint32_t);

    assert(offset % width == 0);
    assert(offset < (1 << 12) * width);

    offset /= width;
    put(code, 0b10111101010000000000000000000000 |
                  (static_cast<uint32_t>(sf) << 30) |
                  (static_cast<uint32_t>(offset) << 10) |
                  (static_cast<uint32_t>(rn) << 5) |
                  (static_cast<uint32_t>(rt) << 0));
}

template <typename RegType>
void ldr(std::byte *&code, bool sf, ireg rn, RegType rt) {
    ldr(code, sf, 0, rn, rt);
}

void ret(std::byte *&code) { put(code, 0b11010110010111110000001111000000); }

}; // namespace raw

namespace masm {

void mov(std::byte *&code, uint64_t imm, ireg dst) {
    if (imm == 0) {
        raw::mov(code, true, ireg::xzr, dst);
        return;
    }

    bool keep = false;
    for (size_t i = 0; i < sizeof(uint32_t) && imm; i++) {
        auto literal = imm & 0xffff;
        imm >>= 16;
        raw::mov(code, true, true, keep, i, literal, dst);
        keep = true;
    }
}

void add(std::byte *&code, Arm64::temp_int_manager &intregs, bool sf,
         uint32_t imm, ireg src, ireg dst) {
    if (imm == 0) {
        raw::mov(code, sf, src, dst);
    } else if (imm < 1 << 24) {
        raw::add(code, sf, dst, src, imm & 0xfff);
        imm >>= 12;
        if (imm) {
            raw::add(code, sf, dst, dst, imm, true);
        }
    } else {
        auto tmp = intregs.temporary();
        mov(code, imm, tmp);
        raw::add(code, sf, dst, src, tmp);
    }
}

void sub(std::byte *&code, Arm64::temp_int_manager &intregs, bool sf,
         uint32_t imm, ireg src, ireg dst) {
    if (imm == 0) {
        raw::mov(code, sf, src, dst);
    } else if (imm < 1 << 24) {
        raw::sub(code, sf, dst, src, imm & 0xfff);
        imm >>= 12;
        if (imm) {
            raw::sub(code, sf, dst, dst, imm, true);
        }
    } else {
        auto tmp = intregs.temporary();
        mov(code, imm, tmp);
        raw::sub(code, sf, dst, src, tmp);
    }
}

template <typename RegType>
void ldr(std::byte *&code, Arm64::temp_int_manager &intregs, bool sf,
         uint32_t offset, ireg rn, RegType rt) {
    if (offset < 1 << 12) {
        raw::ldr(code, sf, offset, rn, rt);
    } else if (offset < 1 << 24) {
        raw::add(code, true, rn, rn, offset >> 12, true);
        raw::ldr(code, sf, offset & 0xfff, rn, rt);
        raw::sub(code, true, rn, rn, offset >> 12, true);
    } else {
        auto offsetreg = intregs.temporary();
        masm::mov(code, offset, offsetreg);
        raw::load(code, memtype::x, resexttype::uns, indexttype::lsl, false,
                  offsetreg, rn, rt);
    }
}

template <typename RegType>
void str_no_temp(std::byte *&code, bool sf, uint32_t offset, ireg rn,
                 RegType rt) {
    assert(offset < (1 << 24));

    if (offset < 1 << 12) {
        raw::str(code, sf, offset, rn, rt);
    } else if (offset < 1 << 24) {
        raw::add(code, true, rn, rn, offset >> 12, true);
        raw::str(code, sf, offset & 0xfff, rn, rt);
        raw::sub(code, true, rn, rn, offset >> 12, true);
    }
}

template <typename RegType>
void str(std::byte *&code, Arm64::temp_int_manager &intregs, bool sf,
         uint32_t offset, ireg rn, RegType rt) {
    if (offset < 1 << 12) {
        raw::str(code, sf, offset, rn, rt);
    } else if (offset < 1 << 24) {
        raw::add(code, true, rn, rn, offset >> 12, true);
        raw::str(code, sf, offset & 0xfff, rn, rt);
        raw::sub(code, true, rn, rn, offset >> 12, true);
    } else {
        auto offsetreg = intregs.temporary();
        masm::mov(code, offset, offsetreg);
        raw::load(code, memtype::x, resexttype::str, indexttype::lsl, false,
                  offsetreg, rn, rt);
    }
}

template <size_t N>
std::array<std::byte *, N> trap(std::byte *&code,
                                std::array<runtime::TrapKind, N> kinds) {
    static_assert(N > 0);

    auto first = code;
    // move first kind to x0
    masm::mov(code, static_cast<uint64_t>(kinds.front()), ireg::x0);
    auto trap = code;
    // put address of runtime::trap in x1
    masm::mov(code, reinterpret_cast<uint64_t>(&runtime::trap), ireg::x1);
    raw::blr(code, ireg::x1);

    std::array<std::byte *, N> labels = {first};
    for (size_t i = 1; i < N; i++) {
        auto kind = kinds[i];
        labels[i] = code;
        // put kind in x0
        masm::mov(code, static_cast<uint64_t>(kind), ireg::x0);
        raw::b(code, trap - code);
    }
    return labels;
}

void trap(std::byte *&code, runtime::TrapKind kind) { trap<1>(code, {kind}); }

}; // namespace masm

template <size_t Bits, size_t Offset>
void put_immediate(std::byte *base, std::byte *to) {
    constexpr auto align = sizeof(uint32_t);

    auto diff = to - base;
    auto value = diff / align;

    // sign lower int32_t to intBits_t
    auto imm = static_cast<uint32_t>(value);
    imm = imm << (32 - Bits) >> (32 - Bits);

    uint32_t v;
    std::memcpy(&v, base, sizeof(v));

    auto high_len = Offset;
    auto low_len = 32 - high_len - Bits;

    auto low =
        Bits + Offset == 32 ? 0 : (v << (high_len + Bits)) >> (high_len + Bits);
    auto high = (v >> (low_len + Bits)) << (low_len + Bits);

    uint32_t result = high | (imm << low_len) | low;
    std::memcpy(base, &result, sizeof(result));
}

// based on
// https://dougallj.wordpress.com/2021/10/30/bit-twiddling-optimising-aarch64-logical-immediate-encoding-and-decoding/
constexpr std::optional<LogicalImm> tryLogicalImm(uint64_t val) {
    if (val == 0 || ~val == 0)
        return std::nullopt;

    uint32_t rotation = std::countr_zero(val & (val + 1));
    uint64_t normalized = std::rotr(val, rotation & 63);

    uint32_t zeroes = std::countl_zero(normalized);
    uint32_t ones = std::countr_one(normalized);
    uint32_t size = zeroes + ones;

    if (std::rotr(val, size & 63) != val)
        return std::nullopt;

    return LogicalImm(size >> 6, -rotation & (size - 1),
                      (-(size << 1) | (ones - 1)) & 0x3f);
}

constexpr std::optional<LogicalImm> tryLogicalImm(uint32_t val) {
    uint64_t val64 = ((uint64_t)val << 32) | val;
    return tryLogicalImm(val64);
}

bool is_volatile(ireg reg) { return reg <= icaller_saved.back(); }
bool is_volatile(freg reg) { return reg <= fcaller_saved.back(); }

}; // namespace

template <auto registers>
void Arm64::temp_reg_manager<registers>::spill(RegType reg) {
    auto [addr, v, offset] = data[to_index(reg)];
    if (addr) {
        masm::str_no_temp(addr, true, offset, stackreg, reg);
        *v = value::stack(offset);
    }
}

template <auto registers>
uint8_t Arm64::temp_reg_manager<registers>::to_index(RegType reg) {
    return static_cast<uint8_t>(reg) - First;
}

template <auto registers>
decltype(registers)::value_type
Arm64::temp_reg_manager<registers>::from_index(uint8_t idx) {
    return static_cast<RegType>(idx + First);
}

template <auto registers> void Arm64::temp_reg_manager<registers>::begin() {
    regs.begin();
}

template <auto registers>
decltype(registers)::value_type Arm64::temp_reg_manager<registers>::result() {
    auto idx = regs.result();
    spill(from_index(idx));
    return from_index(idx);
}

template <auto registers>
void Arm64::temp_reg_manager<registers>::claim(RegType reg, metadata md) {
    auto idx = to_index(reg);
    data[idx] = md;
}

template <auto registers>
decltype(registers)::value_type
Arm64::temp_reg_manager<registers>::temporary() {
    auto idx = regs.temporary();
    spill(from_index(idx));
    data[idx] = metadata(nullptr, 0);
    return from_index(idx);
}

template <auto registers>
void Arm64::temp_reg_manager<registers>::surrender(RegType reg) {
    auto idx = to_index(reg);
    regs.surrender(idx);
    data[idx] = metadata(nullptr, 0);
}

template <auto registers> void Arm64::temp_reg_manager<registers>::commit() {
    regs.commit();
}

template <auto registers>
void Arm64::temp_reg_manager<registers>::clobber_all() {
    for (auto reg : registers) {
        spill(reg);
        surrender(reg);
    }
}

template <auto registers>
bool Arm64::temp_reg_manager<registers>::check_spill(RegType reg,
                                                     std::byte *code) {
    return data[to_index(reg)].spilladdr == code;
}

template <typename RegType, size_t N>
void Arm64::lasting_reg_manager<RegType, N>::spill(RegType reg, size_t i) {
    auto [addr, v, offset] = data[i];
    if (addr) {
        masm::str_no_temp(addr, true, offset, stackreg, reg);
        *v = value::stack(offset);
    }
}

template <typename RegType, size_t N>
void Arm64::lasting_reg_manager<RegType, N>::claim(RegType reg, metadata md) {
    spill(reg, count % N);
    data[count % N] = md;
    count++;
}

template <typename RegType, size_t N>
void Arm64::lasting_reg_manager<RegType, N>::surrender(value *v) {
    assert(count > 0);
    count--;
    assert(data[count % N].value_offset == v);
    data[count % N] = metadata(nullptr, nullptr, 0);
}

template <typename RegType, size_t N>
void Arm64::lasting_reg_manager<RegType, N>::purge(RegType reg) {
    for (auto i = 0; i < std::min(count, N); i++) {
        spill(reg, i);
        data[i] = metadata(nullptr, nullptr, 0);
    }
    count = 0;
}

template <typename RegType, size_t N>
bool Arm64::lasting_reg_manager<RegType, N>::check_spill(RegType reg,
                                                         std::byte *code) {
    assert(count > 0);
    return data[(count - 1) % N].spilladdr == code;
}

template <auto registers>
void Arm64::local_manager<registers>::claim(RegType reg,
                                            sub_manager::metadata md) {
    assert(std::ranges::find(registers, reg) != registers.end());
    get_manager_of(reg).claim(reg, md);
}

template <auto registers>
void Arm64::local_manager<registers>::surrender(RegType reg, value *v) {
    assert(std::ranges::find(registers, reg) != registers.end());
    get_manager_of(reg).surrender(v);
}

template <auto registers>
void Arm64::local_manager<registers>::purge(RegType reg) {
    assert(std::ranges::find(registers, reg) != registers.end());
    get_manager_of(reg).purge(reg);
}

template <auto registers>
bool Arm64::local_manager<registers>::check_spill(RegType reg,
                                                  std::byte *code) {
    assert(std::ranges::find(registers, reg) != registers.end());
    return get_manager_of(reg).check_spill(reg, code);
}

void Arm64::clobber_flags(std::byte *&code) {
    if (!flag.val)
        return;

    // todo: this should go into a .result register (and stay there)
    // but flag clobbering has to happen first thing after allocate_registers
    // so the cset into the .result register could clobber a real value

    // step 1. claim a register, spilling if necessary
    auto reg = intregs.temporary();
    // step 2. spill into claimed register
    raw::cset(code, false, flag.val->as<cond>(), reg);
    // step 3. spill into memory
    masm::str(code, intregs, true, flag.stack_offset, stackreg, reg);

    *flag.val = value::stack(flag.stack_offset);
    flag = flags();
}

void Arm64::clobber_registers() {
    intregs.clobber_all();
    floatregs.clobber_all();
}

void Arm64::push(value v) {
    *values++ = v;
    if (v.is<value::location::flags>())
        flag = flags(stack_size, values - 1);
    stack_size += sizeof(runtime::WasmValue);
}

template <typename To> value Arm64::adapt_value(std::byte *&code, value *v) {
    using RegType =
        std::conditional_t<std::is_same_v<To, iwant::freg>, freg, ireg>;

    switch (v->where()) {
    case value::location::reg: {
        auto r = v->as<RegType>();
        if (is_volatile(r)) {
            if constexpr (std::is_same_v<To, iwant::freg>) {
                if (floatregs.check_spill(r, code - sizeof(inst)))
                    code -= sizeof(inst);
                floatregs.surrender(r);
            } else {
                if (intregs.check_spill(r, code - sizeof(inst)))
                    code -= sizeof(inst);
                intregs.surrender(r);
            }
        } else {
            if constexpr (std::is_same_v<To, iwant::freg>) {
                if (floatlocals.check_spill(r, code - sizeof(inst)))
                    code -= sizeof(inst);
                floatlocals.surrender(r, v);
            } else {
                if (intlocals.check_spill(r, code - sizeof(inst)))
                    code -= sizeof(inst);
                intlocals.surrender(r, v);
            }
        }
        return *v;
    }
    case value::location::stack: {
        auto offset = v->as<uint32_t>();
        RegType reg;
        if constexpr (std::is_same_v<To, iwant::freg>)
            reg = floatregs.temporary();
        else
            reg = intregs.temporary();
        masm::ldr(code, intregs, true, offset, stackreg, reg);
        return value::reg(reg);
    }
    case value::location::imm: {
        auto better_not = !std::is_same_v<To, iwant::freg>;
        assert(better_not);

        auto imm = v->as<uint32_t>();

        if constexpr (is_value_specialization_of<iwant::literal, To>)
            if (imm < To::threshold)
                return *v;
        if constexpr (is_specialization_of<iwant::bitmask, To>)
            if (auto mask = tryLogicalImm((typename To::type)imm))
                return value::imm(std::bit_cast<uint32_t>(*mask));

        auto reg = intregs.temporary();
        masm::mov(code, imm, reg);
        return value::reg(reg);
    }
    case value::location::flags: {
        auto better_not = !std::is_same_v<To, iwant::freg>;
        assert(better_not);

        flag = flags();

        if constexpr (std::is_same_v<To, iwant::flags>) {
            return *v;
        } else {
            auto reg = intregs.temporary();
            raw::cset(code, false, v->as<cond>(), reg);
            return value::reg(reg);
        }
    }
    }

    assert(false);
}

ireg Arm64::adapt_value_into(std::byte *&code, value *v,
                             std::optional<ireg> &hint, bool soft) {
    if (v->is<value::location::reg>()) {
        if (!soft) {
        if (is_volatile(v->as<ireg>()))
            intregs.surrender(v->as<ireg>());
            else
                intlocals.surrender(v->as<ireg>(), v);
        }
        return v->as<ireg>();
    }

    if (!hint)
        hint = intregs.temporary();
    auto reg = *hint;

    switch (v->where()) {
    case value::location::reg: {
        __builtin_unreachable();
    }
    case value::location::stack: {
        auto offset = v->as<uint32_t>();
        masm::ldr(code, intregs, true, offset, stackreg, reg);
        return reg;
    }
    case value::location::imm: {
        auto imm = v->as<uint32_t>();
        masm::mov(code, imm, reg);
        return reg;
    }
    case value::location::flags: {
        if (!soft)
        flag = flags();
        raw::cset(code, false, v->as<cond>(), reg);
        return reg;
    }
    }

    assert(false);
}

freg Arm64::adapt_value_into(std::byte *&code, value *v,
                             std::optional<freg> &hint, bool soft) {
    if (v->is<value::location::reg>()) {
        if (!soft) {
        if (is_volatile(v->as<freg>()))
            floatregs.surrender(v->as<freg>());
            else
                floatlocals.surrender(v->as<freg>(), v);
        }
        return v->as<freg>();
    }

    if (!hint)
        hint = floatregs.temporary();
    auto reg = *hint;

    switch (v->where()) {
    case value::location::reg: {
        __builtin_unreachable();
    }
    case value::location::stack: {
        auto offset = v->as<uint32_t>();
        masm::ldr(code, intregs, true, offset, stackreg, reg);
        return reg;
    }
    case value::location::imm:
    case value::location::flags:
        __builtin_unreachable();
    }

    assert(false);
}

void Arm64::stackify(std::byte *&code, valtype_vector &moved_values) {
    move_results(code, moved_values, stack_size - moved_values.bytesize(),
                 true);
}

bool Arm64::move_results(std::byte *&code, valtype_vector &copied_values,
                         uint32_t stack_offset, bool discard_copied) {
    std::optional<ireg> intreg = std::nullopt;
    std::optional<freg> floatreg = std::nullopt;

    auto start = code;

    auto expected = values - copied_values.size();
    auto dest = stack_offset;
    for (size_t i = 0; i < copied_values.size(); i++) {
        if (expected[i].is<value::location::stack>() &&
            expected[i].as<uint32_t>() == dest) {
            dest += sizeof(runtime::WasmValue);
            continue;
        }

        auto v = copied_values[i];
        if (v == valtype::f32 || v == valtype::f64) {
            auto reg =
                adapt_value_into(code, &expected[i], floatreg, !discard_copied);
            masm::str(code, intregs, true, dest, stackreg, reg);
            dest += sizeof(runtime::WasmValue);
        } else {
            auto reg =
                adapt_value_into(code, &expected[i], intreg, !discard_copied);
            masm::str(code, intregs, true, dest, stackreg, reg);
            dest += sizeof(runtime::WasmValue);
        }
    }

    auto has_move = start != code;

    if (!discard_copied)
        return has_move;

    values -= copied_values.size();
    stack_size -= copied_values.bytesize();

    return has_move;
}

void Arm64::discard(std::byte *&code, WasmStack &stack, uint32_t skip,
                    uint32_t to) {
    auto discarded = (stack_size - to) / sizeof(runtime::WasmValue);

    auto excess = stack.rbegin() + skip;
    for (size_t i = 0; i < discarded; i++) {
        drop(code, stack, *excess);
        excess++;
    }
}

void Arm64::pad_spill(std::byte *&code, uint32_t stack_size) {
    if (stack_size < 1 << 12) {
        put(code, noop);
    } else {
        put(code, noop);
        put(code, noop);
        put(code, noop);
    }
}

void Arm64::amend_br(std::byte *br, std::byte *target) {
    put_immediate<26, 6>(br, target);
}

void Arm64::amend_br_if(std::byte *br, std::byte *target) {
    inst instruction;
    std::memcpy(&instruction, br, sizeof(instruction));
    // cbnz/cbz/b.cond have one of bits 30 and 29 set
    constexpr uint32_t is_conditional = (1 << 30) | (1 << 29);
    if (instruction & is_conditional) {
        put_immediate<19, 8>(br, target);
    } else {
        amend_br(br, target);
    }
}

template <typename Params, typename Result = Arm64::iwant::none>
std::array<value, std::tuple_size_v<Params> +
                      !std::is_same_v<Result, Arm64::iwant::none>>
Arm64::allocate_registers(std::byte *&code) {
    constexpr auto nparams = std::tuple_size_v<Params>;
    std::array<value, nparams + !std::is_same_v<Result, Arm64::iwant::none>>
        ret;

    intregs.begin();
    floatregs.begin();

    values -= nparams;
    stack_size -= sizeof(runtime::WasmValue) * nparams;

    [&]<std::size_t... I>(std::index_sequence<I...>) {
        // going in reverse order allows for both parameters
        //  to be hit by check_spill optimizations
        ((ret[nparams - I - 1] =
              adapt_value<std::tuple_element_t<nparams - I - 1, Params>>(
                  code, &values[nparams - I - 1])),
         ...);
    }(std::make_index_sequence<nparams>{});

    if constexpr (std::is_same_v<Result, iwant::ireg>) {
        ret.back() = value::reg(intregs.result());
    } else if constexpr (std::is_same_v<Result, iwant::freg>) {
        ret.back() = value::reg(floatregs.result());
    } else {
        static_assert(std::is_same_v<Result, iwant::none>);
    }

    return ret;
}

template <typename... Args>
void Arm64::finalize(std::byte *&code, Args... results) {
    auto finalize = [&](auto result) {
        if constexpr (std::is_same_v<decltype(result), ireg>)
            intregs.claim(result, {code, values, stack_size});
        else
            floatregs.claim(result, {code, values, stack_size});

        inst instruction;
        std::memcpy(&instruction, code - sizeof(inst), sizeof(inst));
        assert((instruction & 0b11111u) == (unsigned)result);

        pad_spill(code, stack_size);
        push(value::reg(result));
    };

    (finalize(results), ...);

    // always commit intregs because there could be a spilled flag
    // this could be optimized but like i really doubt it's a bottleneck
    intregs.commit();
    if constexpr ((std::is_same_v<Args, freg> || ...))
        floatregs.commit();
}

void Arm64::start_function(SHARED_PARAMS, FunctionShell &fn) {
    raw::stp(code, true, enctype::preidx, -0x10, ireg::x30, ireg::sp,
             ireg::x29);
    raw::add(code, true, ireg::x29, ireg::sp, 0);

    locals = std::span(new value[fn.locals.size()], fn.locals.size());

    auto ireg_alloc = icallee_saved.begin();
    auto freg_alloc = fcallee_saved.begin();

    for (size_t i = 0; i < fn.locals.size(); i++) {
        auto local = fn.locals[i];
        auto offset = i * sizeof(runtime::WasmValue);
        auto is_param = i < fn.type.params.size();

        if ((local == valtype::i32 || local == valtype::i64 ||
             local == valtype::funcref || local == valtype::externref) &&
            ireg_alloc != icallee_saved.end()) {
            auto reg = *ireg_alloc++;
            // save current value
            if (is_param) {
                raw::mov(code, true, reg, ireg::x3);
                masm::ldr(code, intregs, true, offset, stackreg, reg);
                masm::str(code, intregs, true, offset, stackreg, ireg::x3);
            } else {
                masm::str(code, intregs, true, offset, stackreg, reg);
                raw::mov(code, true, ireg::xzr, reg);
            }

            locals[i] = value::reg(reg);
        } else if ((local == valtype::f32 || local == valtype::f64) &&
                   freg_alloc != fcallee_saved.end()) {
            auto reg = *freg_alloc++;
            // save current value
            if (is_param) {
                raw::mov(code, ftype::double_, reg, freg::d0);
                masm::ldr(code, intregs, true, offset, stackreg, reg);
                masm::str(code, intregs, true, offset, stackreg, freg::d0);
            } else {
                masm::str(code, intregs, true, offset, stackreg, reg);
                raw::mov(code, true, ftype::double_, ireg::xzr, reg);
            }

            locals[i] = value::reg(reg);
        } else {
            if (!is_param) {
                masm::str(code, intregs, true, offset, stackreg, ireg::xzr);
            }

            locals[i] = value::stack(offset);
        }
    }

    stack_size = fn.locals.size() * sizeof(runtime::WasmValue);
}
void Arm64::exit_function(SHARED_PARAMS, ControlFlow &flow) {
    // note: this has to be fixed to not potentially overwrite locals
    // that are being returned

    auto &fn = std::get<Function>(flow.construct).fn;
    // restore saved values
    for (size_t i = 0; i < fn.locals.size(); i++) {
        if (!locals[i].is<value::location::reg>())
            continue;

        auto local = fn.locals[i];
        auto offset = i * sizeof(runtime::WasmValue);

        if (local == valtype::i32 || local == valtype::i64 ||
            local == valtype::funcref || local == valtype::externref) {
            masm::ldr(code, intregs, true, offset, stackreg,
                      locals[i].as<ireg>());
        } else if (local == valtype::f32 || local == valtype::f64) {
            masm::ldr(code, intregs, true, offset, stackreg,
                      locals[i].as<freg>());
        }
    }

    if (auto local_bytes = fn.locals.bytesize(); local_bytes != 0) {
        // return values should be in [local_bytes, ...), so copy them backwards
        // into the local area
        // clobber at will (in this case x3)
        for (size_t i = 0; i < fn.type.results.size(); i++) {
            auto final_offset = i * sizeof(runtime::WasmValue);
            auto current_offset = local_bytes + final_offset;

            masm::ldr(code, intregs, true, current_offset, stackreg, ireg::x3);
            masm::str(code, intregs, true, final_offset, stackreg, ireg::x3);
        }
    }

    raw::ldp(code, true, enctype::pstidx, 0x10, ireg::x30, ireg::sp, ireg::x29);
    raw::ret(code);

    delete[] locals.data();
}

void Arm64::unreachable(SHARED_PARAMS) {
    auto v = stack.rbegin();
    while (*v != valtype::null) {
        drop(code, stack, *v);
        v++;
    }

    masm::trap(code, runtime::TrapKind::unreachable);
}
void Arm64::nop(SHARED_PARAMS) { put(code, noop); }
void Arm64::block(SHARED_PARAMS, WasmSignature &) {}
void Arm64::loop(SHARED_PARAMS, WasmSignature &sig) {
    stackify(code, sig.params);

    for ([[maybe_unused]] auto param : sig.params) {
        push(value::stack(stack_size));
    }
}
std::byte *Arm64::if_(SHARED_PARAMS, WasmSignature &sig) {
    auto [condition] = allocate_registers<std::tuple<iwant::flags>>(code);

    auto dupe = values - sig.params.size();
    for (size_t i = 0; i < sig.params.size(); i++) {
        // this is super hacky, but instead of dumping the values to the stack
        // and then loading from stack for both if/else blocks, the params will
        // already be on the top of the stack after dumping results of the if

        // should make sure i don't assume values translates 1:1 with stack_size
        *values++ = dupe[i];
    }

    std::byte *imm = code;
    if (condition.is<value::location::flags>()) {
        raw::bcond(code, 0, invert(condition.as<cond>()));
    } else {
        raw::cbz(code, false, 0, condition.as<ireg>());
    }

    return imm;
}
void Arm64::else_(SHARED_PARAMS, std::span<ControlFlow> control_stack) {
    // todo: this is a tiny bit efficient, because at this point we have the
    // additional knowledge that there is no need to move the results
    // however it should be possible to optimize that anyways in move_results
    if (!stack.polymorphism()) {
        br(code, stack, control_stack, 0);
    }

    auto &if_flow = control_stack.back();
    amend_br_if(std::get<If>(if_flow.construct).else_jump, code);

    stack_size += if_flow.sig.params.bytesize();

    // not needed due to duping in if_
    // for (auto ty : if_flow.sig.params)
    //     push(value::stack(stack_size));
}
void Arm64::end(SHARED_PARAMS, ControlFlow &flow) {
    intregs.begin();
    floatregs.begin();

    if (!stack.polymorphism()) {
        stackify(code, flow.sig.results);
    }

    if (std::holds_alternative<If>(flow.construct)) {
        amend_br_if(std::get<If>(flow.construct).else_jump, code);

        // move the duplicated params into their proper positions
        stack_size += flow.sig.params.bytesize();
        stackify(code, flow.sig.results);
    }

    for ([[maybe_unused]] auto result : flow.sig.results) {
        push(value::stack(stack_size));
    }

    if (!std::holds_alternative<Loop>(flow.construct)) {
        for (auto target : flow.pending_br) {
            amend_br(target, code);
        }
        for (auto target : flow.pending_br_if) {
            amend_br_if(target, code);
        }
        for (auto [table, target] : flow.pending_br_tables) {
            auto diff = code - table;
            auto idiff = static_cast<int32_t>(diff);
            ensure(idiff == diff, "branch target out of range");
            std::memcpy(target, &idiff, sizeof(idiff));
        }
    }

    if (std::holds_alternative<Function>(flow.construct)) {
        exit_function(code, stack, flow);
    }
}
void Arm64::br(SHARED_PARAMS, std::span<ControlFlow> control_stack,
               uint32_t depth) {
    // for now, only support non-special case distances (+/- 128MB)

    // todo: check if these are necessary, or can they go back in move_results
    intregs.begin();
    floatregs.begin();

    auto &flow = control_stack[control_stack.size() - depth - 1];

    move_results(code, flow.expected, flow.stack_offset, true);
    discard(code, stack, flow.expected.size(),
            control_stack.back().stack_offset);

    auto imm = code;
    raw::b(code, 0);

    if (std::holds_alternative<Loop>(flow.construct)) {
        auto start = std::get<Loop>(flow.construct).start;
        amend_br(imm, start);
    } else {
        flow.pending_br.push_back(imm);
    }
}
void Arm64::br_if(SHARED_PARAMS, std::span<ControlFlow> control_stack,
                  uint32_t depth) {
    // for now, only support non-special case distances (+/- 1MB)

    auto &flow = control_stack[control_stack.size() - depth - 1];
    auto [condition] = allocate_registers<std::tuple<iwant::flags>>(code);

    std::byte *condjump = code;
    code += sizeof(inst);

    std::byte *imm;
    if (move_results(code, flow.expected, flow.stack_offset, false)) {
        imm = code;
        raw::b(code, 0);

        auto jump = code - condjump;
        if (condition.is<value::location::flags>()) {
            raw::bcond(condjump, jump, invert(condition.as<cond>()));
        } else {
            raw::cbz(condjump, false, jump, condition.as<ireg>());
        }
    } else {
        imm = condjump;
        if (condition.is<value::location::flags>()) {
            raw::bcond(condjump, 0, condition.as<cond>());
        } else {
            raw::cbnz(condjump, false, 0, condition.as<ireg>());
        }
    }

    if (std::holds_alternative<Loop>(flow.construct)) {
        auto start = std::get<Loop>(flow.construct).start;
        amend_br_if(imm, start);
    } else {
        flow.pending_br_if.push_back(imm);
    }
}
void Arm64::br_table(SHARED_PARAMS, std::span<ControlFlow> control_stack,
                     std::span<uint32_t> targets) {
    auto [input] = allocate_registers<std::tuple<iwant::ireg>>(code);

    clobber_flags(code);

    auto base = control_stack.size() - 1;
    auto &wanted = control_stack[base - targets.back()].expected;

    auto depth = intregs.temporary();
    auto addr = intregs.temporary();

    // put max depth (-1 for default target) in $depth
    masm::mov(code, targets.size() - 1, depth);
    // $depth = min($depth, $input)
    raw::cmp(code, false, input.as<ireg>(), depth);
    raw::csel(code, false, depth, cond::cc, input.as<ireg>(), depth);
    // put table address in $addr
    auto adr_location = code;
    code += sizeof(inst);
    // $addr = $addr + $depth * sizeof(BrTableTarget)
    raw::add(code, true, addr, depth, addr, shifttype::lsl, 3);
    // reuse $addr & $depth into $result_offset & $jump_offset
    auto result_offset = addr, jump_offset = depth;
    // [$result_offset, $jump_offset] = ldpsw($addr)
    raw::ldpsw(code, 0, result_offset, addr, jump_offset);

    std::optional<ireg> intreg = std::nullopt;
    std::optional<freg> floatreg = std::nullopt;

    auto expected = values - wanted.size();
    for (size_t i = 0; i < wanted.size(); i++) {
        auto v = wanted[i];
        if (v == valtype::f32 || v == valtype::f64) {
            auto reg = adapt_value_into(code, &expected[i], floatreg);
            raw::load(code, memtype::x, resexttype::str, indexttype::lsl, false,
                      result_offset, stackreg, reg);
            raw::add(code, true, result_offset, result_offset,
                     sizeof(runtime::WasmValue));
        } else {
            auto reg = adapt_value_into(code, &expected[i], intreg);
            raw::load(code, memtype::x, resexttype::str, indexttype::lsl, false,
                      result_offset, stackreg, reg);
            raw::add(code, true, result_offset, result_offset,
                     sizeof(runtime::WasmValue));
        }
    }

    values -= wanted.size();
    stack_size -= wanted.bytesize();

    discard(code, stack, wanted.size(), control_stack.back().stack_offset);

    auto relative_point = code;
    // $jump = PC + $jump_offset
    auto jump = result_offset;
    raw::adr(code, 0, jump);
    raw::add(code, true, jump, jump, jump_offset);
    raw::br(code, jump);

    raw::adr(adr_location, code - adr_location, addr);

    for (auto depth : targets) {
        auto &flow = control_stack[base - depth];
        auto offset = flow.stack_offset;
        if (std::holds_alternative<Loop>(flow.construct)) {
            auto target = runtime::BrTableTarget(
                std::get<Loop>(flow.construct).start - relative_point, offset);
            std::memcpy(code, &target, sizeof(target));
            code += sizeof(target);
        } else {
            flow.pending_br_tables.push_back(
                PendingBrTable(relative_point, code));
            code += sizeof(uint32_t);
            std::memcpy(code, &offset, sizeof(offset));
            code += sizeof(offset);
        }
    }

    finalize(code);
}
void Arm64::return_(SHARED_PARAMS, std::span<ControlFlow> control_stack) {
    br(code, stack, control_stack, control_stack.size() - 1);
}
void Arm64::call(SHARED_PARAMS, FunctionShell &fn, uint32_t func_offset) {
    allocate_registers<std::tuple<>>(code);

    auto exhaust_ptr = intregs.temporary(), exhaust = intregs.temporary(),
         function_ptr = exhaust_ptr, signature = exhaust;

    clobber_flags(code);
    clobber_registers();

    stackify(code, fn.type.params);

    masm::mov(code, reinterpret_cast<uint64_t>(&runtime::call_stack_depth),
              exhaust_ptr);
    raw::ldr(code, false, exhaust_ptr, exhaust);
    raw::stp(code, true, enctype::preidx, -2 * (int)sizeof(uint64_t), exhaust,
             ireg::sp, exhaust_ptr);
    raw::subs(code, false, exhaust, exhaust, 1);
    raw::str(code, false, exhaust_ptr, exhaust);

    auto exhaustion = code;
    raw::bcond(code, 0, cond::ne);
    masm::trap(code, runtime::TrapKind::call_stack_exhausted);
    amend_br_if(exhaustion, code);

    // load the FunctionInfo pointer
    masm::ldr(code, intregs, true, func_offset * sizeof(void *), miscreg,
              function_ptr);
    if (fn.import) {
        static_assert(offsetof(runtime::FunctionInfo, memory) +
                          sizeof(void *) ==
                      offsetof(runtime::FunctionInfo, misc));

        raw::stp(code, true, enctype::preidx, -2 * (int)sizeof(uint64_t),
                 miscreg, ireg::sp, memreg);
        raw::ldp(code, true, enctype::offset,
                 offsetof(runtime::FunctionInfo, memory), miscreg, function_ptr,
                 memreg);
    }

    raw::ldr(code, true, offsetof(runtime::FunctionInfo, signature),
             function_ptr, signature);

    masm::add(code, intregs, true, stack_size, stackreg, stackreg);
    raw::blr(code, signature);
    masm::sub(code, intregs, true, stack_size, stackreg, stackreg);

    if (fn.import) {
        raw::ldp(code, true, enctype::pstidx, 2 * sizeof(uint64_t), miscreg,
                 ireg::sp, memreg);
    }

    raw::ldp(code, true, enctype::pstidx, 2 * sizeof(uint64_t), exhaust,
             ireg::sp, exhaust_ptr);
    raw::str(code, false, exhaust_ptr, exhaust);

    for ([[maybe_unused]] auto result : fn.type.results) {
        push(value::stack(stack_size));
    }

    finalize(code);
}
void Arm64::call_indirect(SHARED_PARAMS, uint32_t table_offset,
                          WasmSignature &type) {
    auto [v] = allocate_registers<std::tuple<iwant::ireg>>(code);
    auto idx = v.as<ireg>();
    auto table_ptr = intregs.temporary(), current = intregs.temporary(),
         elements = intregs.temporary(), function_ptr = intregs.temporary(),
         expected_sig = intregs.temporary(), given_sig = intregs.temporary(),
         exhaust_ptr = intregs.temporary(), exhaust = intregs.temporary();

    clobber_flags(code);
    clobber_registers();

    stackify(code, type.params);

    masm::mov(code, reinterpret_cast<uint64_t>(&runtime::call_stack_depth),
              exhaust_ptr);
    raw::ldr(code, false, exhaust_ptr, exhaust);
    raw::stp(code, true, enctype::preidx, -2 * (int)sizeof(uint64_t), exhaust,
             ireg::sp, exhaust_ptr);
    raw::subs(code, false, exhaust, exhaust, 1);
    raw::str(code, false, exhaust_ptr, exhaust);

    auto exhaustion = code;
    raw::bcond(code, 0, cond::ne);
    masm::trap(code, runtime::TrapKind::call_stack_exhausted);
    amend_br_if(exhaustion, code);

    masm::ldr(code, intregs, true, table_offset * sizeof(void *), miscreg,
              table_ptr);

    static_assert(offsetof(runtime::WasmTable, current) == 0);
    static_assert(offsetof(runtime::WasmTable, elements) == 8);
    raw::ldp(code, true, enctype::offset, 0, elements, table_ptr, current);

    raw::cmp(code, false, current, idx);
    auto undefined_trap = code;
    raw::bcond(code, 0, cond::ls);

    raw::load(code, memtype::x, resexttype::uns, indexttype::lsl, true, idx,
              elements, function_ptr);

    auto uninitialized_trap = code;
    raw::cbz(code, true, 0, function_ptr);

    auto rttype = runtime::FunctionType(type);
    uint64_t p1;
    uint32_t p2;
    std::memcpy(&p1, &rttype, sizeof(p1));
    std::memcpy(&p2, (char *)&rttype + sizeof(p1), sizeof(p2));
    static_assert(sizeof(p1) + sizeof(p2) == sizeof(rttype));

    masm::mov(code, p1, expected_sig);
    raw::ldr(code, true, offsetof(runtime::FunctionInfo, type), function_ptr,
             given_sig);
    raw::cmp(code, true, expected_sig, given_sig);

    masm::mov(code, p2, expected_sig);
    raw::ldr(code, false, offsetof(runtime::FunctionInfo, type) + sizeof(p1),
             function_ptr, given_sig);

    raw::ccmp(code, false, expected_sig, cond::eq, given_sig, 0);
    auto type_mismatch_trap = code;
    raw::bcond(code, 0, cond::ne);

    raw::stp(code, true, enctype::preidx, -2 * (int)sizeof(uint64_t), miscreg,
             ireg::sp, memreg);
    raw::ldp(code, true, enctype::offset,
             offsetof(runtime::FunctionInfo, memory), miscreg, function_ptr,
             memreg);

    raw::ldr(code, true, offsetof(runtime::FunctionInfo, signature),
             function_ptr, function_ptr);

    // todo: make this work with stack_size larger than 1 << 12
    masm::add(code, intregs, true, stack_size, stackreg, stackreg);
    raw::blr(code, function_ptr);
    masm::sub(code, intregs, true, stack_size, stackreg, stackreg);

    raw::ldp(code, true, enctype::pstidx, 2 * sizeof(uint64_t), miscreg,
             ireg::sp, memreg);

    raw::ldp(code, true, enctype::pstidx, 2 * sizeof(uint64_t), exhaust,
             ireg::sp, exhaust_ptr);
    raw::str(code, false, exhaust_ptr, exhaust);

    auto traps = code;
    raw::b(code, 0);

    auto [undefined_loc, uninitialized_loc, type_mismatch_loc] =
        masm::trap(code, std::to_array({
                             runtime::TrapKind::undefined_element,
                             runtime::TrapKind::uninitialized_element,
                             runtime::TrapKind::indirect_call_type_mismatch,
                         }));
    amend_br_if(undefined_trap, undefined_loc);
    amend_br_if(uninitialized_trap, uninitialized_loc);
    amend_br_if(type_mismatch_trap, type_mismatch_loc);

    amend_br(traps, code);

    for ([[maybe_unused]] auto result : type.results) {
        push(value::stack(stack_size));
    }

    finalize(code);
}
void Arm64::drop(SHARED_PARAMS, valtype type) {
    values--;
    stack_size -= sizeof(runtime::WasmValue);

    switch (values->where()) {
    case value::location::reg:
        if (type == valtype::f32 || type == valtype::f64) {
            auto r = values->as<freg>();
            if (is_volatile(r)) {
                floatregs.surrender(r);
            } else {
                floatlocals.surrender(r, values);
            }
        } else {
            auto r = values->as<ireg>();
            if (is_volatile(r)) {
                intregs.surrender(r);
            } else {
                intlocals.surrender(r, values);
            }
        }
        break;
    case value::location::stack:
        break;
    case value::location::imm:
        break;
    case value::location::flags:
        flag = flags();
        break;
    }
}
void Arm64::select(SHARED_PARAMS, valtype type) {
    if (type == valtype::f32 || type == valtype::f64) {
        auto [v1, v2, condition, res] = allocate_registers<
            std::tuple<iwant::freg, iwant::freg, iwant::flags>, iwant::freg>(
            code);

        if (!condition.is<value::location::flags>()) {
            clobber_flags(code);
            raw::cmp(code, false, condition.as<ireg>(), ireg::xzr);
            condition = value::flag(cond::ne);
        }

        raw::csel(code, type != valtype::f32, v2.as<freg>(),
                  condition.as<cond>(), v1.as<freg>(), res.as<freg>());

        finalize(code, res.as<freg>());
    } else {
        auto [v1, v2, condition, res] = allocate_registers<
            std::tuple<iwant::ireg, iwant::ireg, iwant::flags>, iwant::ireg>(
            code);

        if (!condition.is<value::location::flags>()) {
            clobber_flags(code);
            raw::cmp(code, false, condition.as<ireg>(), ireg::xzr);
            condition = value::flag(cond::ne);
        }

        raw::csel(code, type != valtype::i32, v2.as<ireg>(),
                  condition.as<cond>(), v1.as<ireg>(), res.as<ireg>());

        finalize(code, res.as<ireg>());
    }
}
void Arm64::select_t(SHARED_PARAMS, valtype type) { select(code, stack, type); }
void Arm64::localget(SHARED_PARAMS, FunctionShell &fn, uint32_t local_idx) {
    auto ty = fn.locals[local_idx];
    auto local = locals[local_idx];

    if (local.is<value::location::stack>()) {
        if (ty == valtype::f32 || ty == valtype::f64) {
            auto [reg] = allocate_registers<std::tuple<>, iwant::freg>(code);
            masm::ldr(code, intregs, true, local.as<uint32_t>(), stackreg,
                      reg.as<freg>());
            // todo: this noop move is necessary because efficient local.set
            // handling requires the last instruction to move into the result
            raw::mov(code, ftype::double_, reg.as<freg>(), reg.as<freg>());
            finalize(code, reg.as<freg>());
        } else {
            auto [reg] = allocate_registers<std::tuple<>, iwant::ireg>(code);
            masm::ldr(code, intregs, true, local.as<uint32_t>(), stackreg,
                      reg.as<ireg>());
            raw::mov(code, true, reg.as<ireg>(), reg.as<ireg>());
            finalize(code, reg.as<ireg>());
        }
    } else {
        if (ty == valtype::f32 || ty == valtype::f64) {
            floatlocals.claim(local.as<freg>(), {code, values, stack_size});
        } else {
            intlocals.claim(local.as<ireg>(), {code, values, stack_size});
        }

        pad_spill(code, stack_size);
        push(local);
    }
}
void Arm64::localset(SHARED_PARAMS, FunctionShell &fn, uint32_t local_idx) {
    auto ty = fn.locals[local_idx];
    auto local = locals[local_idx];

    if (local.is<value::location::reg>()) {
        // i don't like that this specializes allocate_registers
        // but tbf it's the only place i need to do it

        allocate_registers<std::tuple<>>(code);

        values -= 1;
        stack_size -= sizeof(runtime::WasmValue);

        if (ty == valtype::f32 || ty == valtype::f64) {
            if (auto reg = values->as<freg>();
                values->is<value::location::reg>() && is_volatile(reg) &&
                floatregs.check_spill(reg, code - sizeof(inst))) {
                code -= 2 * sizeof(inst);

                inst instruction;
                std::memcpy(&instruction, code, sizeof(inst));
                assert((instruction & 0b11111u) ==
                       (unsigned)values->as<freg>());
                instruction &= ~0b11111u;
                instruction |= (unsigned)local.as<freg>();
                put(code, instruction);

                floatregs.surrender(reg);
            } else {
                auto wrapped = std::make_optional(local.as<freg>());
                auto v = adapt_value_into(code, values, wrapped);
                if (*wrapped != v)
                    raw::mov(code, ftype::double_, v, *wrapped);
            }

            // sorry in flight locals! the set has purged the register ;(
            floatlocals.purge(local.as<freg>());
        } else {
            if (auto reg = values->as<ireg>();
                values->is<value::location::reg>() && is_volatile(reg) &&
                intregs.check_spill(reg, code - sizeof(inst))) {
                code -= 2 * sizeof(inst);

                inst instruction;
                std::memcpy(&instruction, code, sizeof(inst));
                assert((instruction & 0b11111u) ==
                       (unsigned)values->as<ireg>());
                instruction &= ~0b11111u;
                instruction |= (unsigned)local.as<ireg>();
                put(code, instruction);

                intregs.surrender(reg);
            } else {
                auto wrapped = std::make_optional(local.as<ireg>());
                auto v = adapt_value_into(code, values, wrapped);
                if (*wrapped != v)
                    raw::mov(code, true, v, *wrapped);
            }

            intlocals.purge(local.as<ireg>());
        }
    } else {
        if (ty == valtype::f32 || ty == valtype::f64) {
            auto [reg] = allocate_registers<std::tuple<iwant::freg>>(code);
            masm::str(code, intregs, true, local.as<uint32_t>(), stackreg,
                      reg.as<freg>());
        } else {
            auto [reg] = allocate_registers<std::tuple<iwant::ireg>>(code);
            masm::str(code, intregs, true, local.as<uint32_t>(), stackreg,
                      reg.as<ireg>());
        }
    }

    finalize(code);
}
void Arm64::localtee(SHARED_PARAMS, FunctionShell &fn, uint32_t local_idx) {
    localset(code, stack, fn, local_idx);
    localget(code, stack, fn, local_idx);
}
void Arm64::tableget(SHARED_PARAMS, uint64_t misc_offset) {
    auto [idx, res] =
        allocate_registers<std::tuple<iwant::ireg>, iwant::ireg>(code);
    auto table_ptr = intregs.temporary(), elements = intregs.temporary(),
         current = table_ptr;

    clobber_flags(code);

    masm::ldr(code, intregs, true, misc_offset * sizeof(void *), miscreg,
              table_ptr);

    static_assert(offsetof(runtime::WasmTable, current) == 0);
    static_assert(offsetof(runtime::WasmTable, elements) == 8);
    raw::ldp(code, true, enctype::offset, 0, elements, table_ptr, current);

    raw::cmp(code, false, idx.as<ireg>(), current);
    auto oob_trap = code;
    raw::bcond(code, 0, cond::cc);
    masm::trap(code, runtime::TrapKind::out_of_bounds_table_access);
    amend_br_if(oob_trap, code);

    raw::load(code, memtype::x, resexttype::uns, indexttype::lsl, true,
              idx.as<ireg>(), elements, res.as<ireg>());

    finalize(code, res.as<ireg>());
}
void Arm64::tableset(SHARED_PARAMS, uint64_t misc_offset) {
    auto [idx, value] =
        allocate_registers<std::tuple<iwant::ireg, iwant::ireg>>(code);
    auto table_ptr = intregs.temporary(), elements = intregs.temporary(),
         current = table_ptr;

    clobber_flags(code);

    masm::ldr(code, intregs, true, misc_offset * sizeof(void *), miscreg,
              table_ptr);

    static_assert(offsetof(runtime::WasmTable, current) == 0);
    static_assert(offsetof(runtime::WasmTable, elements) == 8);
    raw::ldp(code, true, enctype::offset, 0, elements, table_ptr, current);

    raw::cmp(code, false, idx.as<ireg>(), current);
    auto oob_trap = code;
    raw::bcond(code, 0, cond::cc);
    masm::trap(code, runtime::TrapKind::out_of_bounds_table_access);
    amend_br_if(oob_trap, code);

    raw::load(code, memtype::x, resexttype::str, indexttype::lsl, true,
              idx.as<ireg>(), elements, value.as<ireg>());

    finalize(code);
}
void Arm64::globalget(SHARED_PARAMS, uint64_t misc_offset, valtype type) {
    if (type == valtype::f32 || type == valtype::f64) {
        auto [res] = allocate_registers<std::tuple<>, iwant::freg>(code);
        auto addr = intregs.temporary();
        masm::ldr(code, intregs, true, misc_offset * sizeof(void *), miscreg,
                  addr);
        raw::ldr(code, true, addr, res.as<freg>());
        finalize(code, res.as<freg>());
    } else {
        auto [res] = allocate_registers<std::tuple<>, iwant::ireg>(code);
        masm::ldr(code, intregs, true, misc_offset * sizeof(void *), miscreg,
                  res.as<ireg>());
        raw::ldr(code, true, res.as<ireg>(), res.as<ireg>());
        finalize(code, res.as<ireg>());
    }
}
void Arm64::globalset(SHARED_PARAMS, uint64_t misc_offset, valtype type) {
    if (type == valtype::f32 || type == valtype::f64) {
        auto [val] = allocate_registers<std::tuple<iwant::freg>>(code);
        auto addr = intregs.temporary();
        masm::ldr(code, intregs, true, misc_offset * sizeof(void *), miscreg,
                  addr);
        raw::str(code, true, addr, val.as<freg>());
        finalize(code);
    } else {
        auto [val] = allocate_registers<std::tuple<iwant::ireg>>(code);
        auto addr = intregs.temporary();
        masm::ldr(code, intregs, true, misc_offset * sizeof(void *), miscreg,
                  addr);
        raw::str(code, true, addr, val.as<ireg>());
        finalize(code);
    }
}
void Arm64::memorysize(SHARED_PARAMS) {
    auto [res] = allocate_registers<std::tuple<>, iwant::ireg>(code);

    raw::ldr(code, true, miscreg, res.as<ireg>());
    raw::ldr(code, true, offsetof(runtime::WasmMemory, current), res.as<ireg>(),
             res.as<ireg>());

    finalize(code, res.as<ireg>());
}
void Arm64::memorygrow(SHARED_PARAMS) {
    runtime_call<runtime::memorygrow>(code, std::to_array({valtype::i32}),
                                      std::to_array({valtype::i32}));
}
void Arm64::i32const(SHARED_PARAMS, uint32_t cons) { push(value::imm(cons)); }
void Arm64::i64const(SHARED_PARAMS, uint64_t cons) {
    if (cons > std::numeric_limits<uint32_t>::max()) {
        auto [res] = allocate_registers<std::tuple<>, iwant::ireg>(code);
        masm::mov(code, cons, res.as<ireg>());
        raw::mov(code, true, res.as<ireg>(), res.as<ireg>());
        finalize(code, res.as<ireg>());
    } else {
        push(value::imm(cons));
    }
}
void Arm64::f32const(SHARED_PARAMS, float cons) {
    auto [res] = allocate_registers<std::tuple<>, iwant::freg>(code);
    auto temp = intregs.temporary();

    masm::mov(code, std::bit_cast<uint32_t>(cons), temp);
    raw::mov(code, false, ftype::single, temp, res.as<freg>());

    finalize(code, res.as<freg>());
}
void Arm64::f64const(SHARED_PARAMS, double cons) {
    auto [res] = allocate_registers<std::tuple<>, iwant::freg>(code);
    auto temp = intregs.temporary();

    masm::mov(code, std::bit_cast<uint64_t>(cons), temp);
    raw::mov(code, true, ftype::double_, temp, res.as<freg>());

    finalize(code, res.as<freg>());
}

template <memtype mtype, resexttype etype, bool is_float = false>
void Arm64::abstract_memop(SHARED_PARAMS, uint64_t offset) {
    constexpr bool is_store = etype == resexttype::str;

    using IWantTy = std::conditional_t<is_float, iwant::freg, iwant::ireg>;
    using RegTy = std::conditional_t<is_float, freg, ireg>;

    using Result = std::conditional_t<is_store, iwant::none, IWantTy>;
    using Params = decltype(std::tuple_cat(
        std::tuple<iwant::ireg>{},
        std::conditional_t<is_store, std::tuple<IWantTy>, std::tuple<>>{}));

    auto [base, res] = allocate_registers<Params, Result>(code);

    auto addr = base.template as<ireg>();
    if (offset) {
        if (!is_volatile(addr))
            addr = intregs.temporary();
        masm::add(code, intregs, true, offset, base.template as<ireg>(), addr);
    }
    raw::load(code, mtype, etype, indexttype::lsl, false, addr, memreg,
              res.template as<RegTy>());

    if constexpr (!is_store)
        finalize(code, res.template as<RegTy>());
}

void Arm64::i32load(SHARED_PARAMS, uint64_t offset, uint64_t) {
    abstract_memop<memtype::w, resexttype::uns>(code, stack, offset);
}
void Arm64::i64load(SHARED_PARAMS, uint64_t offset, uint64_t) {
    abstract_memop<memtype::x, resexttype::uns>(code, stack, offset);
}
void Arm64::f32load(SHARED_PARAMS, uint64_t offset, uint64_t) {
    abstract_memop<memtype::w, resexttype::uns, true>(code, stack, offset);
}
void Arm64::f64load(SHARED_PARAMS, uint64_t offset, uint64_t) {
    abstract_memop<memtype::x, resexttype::uns, true>(code, stack, offset);
}
void Arm64::i32load8_s(SHARED_PARAMS, uint64_t offset, uint64_t) {
    abstract_memop<memtype::b, resexttype::wse>(code, stack, offset);
}
void Arm64::i32load8_u(SHARED_PARAMS, uint64_t offset, uint64_t) {
    abstract_memop<memtype::b, resexttype::uns>(code, stack, offset);
}
void Arm64::i32load16_s(SHARED_PARAMS, uint64_t offset, uint64_t) {
    abstract_memop<memtype::h, resexttype::wse>(code, stack, offset);
}
void Arm64::i32load16_u(SHARED_PARAMS, uint64_t offset, uint64_t) {
    abstract_memop<memtype::h, resexttype::uns>(code, stack, offset);
}
void Arm64::i64load8_s(SHARED_PARAMS, uint64_t offset, uint64_t) {
    abstract_memop<memtype::b, resexttype::dse>(code, stack, offset);
}
void Arm64::i64load8_u(SHARED_PARAMS, uint64_t offset, uint64_t) {
    abstract_memop<memtype::b, resexttype::uns>(code, stack, offset);
}
void Arm64::i64load16_s(SHARED_PARAMS, uint64_t offset, uint64_t) {
    abstract_memop<memtype::h, resexttype::dse>(code, stack, offset);
}
void Arm64::i64load16_u(SHARED_PARAMS, uint64_t offset, uint64_t) {
    abstract_memop<memtype::h, resexttype::uns>(code, stack, offset);
}
void Arm64::i64load32_s(SHARED_PARAMS, uint64_t offset, uint64_t) {
    abstract_memop<memtype::w, resexttype::dse>(code, stack, offset);
}
void Arm64::i64load32_u(SHARED_PARAMS, uint64_t offset, uint64_t) {
    abstract_memop<memtype::w, resexttype::uns>(code, stack, offset);
}
void Arm64::i32store(SHARED_PARAMS, uint64_t offset, uint64_t) {
    abstract_memop<memtype::w, resexttype::str>(code, stack, offset);
}
void Arm64::i64store(SHARED_PARAMS, uint64_t offset, uint64_t) {
    abstract_memop<memtype::x, resexttype::str>(code, stack, offset);
}
void Arm64::f32store(SHARED_PARAMS, uint64_t offset, uint64_t) {
    abstract_memop<memtype::w, resexttype::str, true>(code, stack, offset);
}
void Arm64::f64store(SHARED_PARAMS, uint64_t offset, uint64_t) {
    abstract_memop<memtype::x, resexttype::str, true>(code, stack, offset);
}
void Arm64::i32store8(SHARED_PARAMS, uint64_t offset, uint64_t) {
    abstract_memop<memtype::b, resexttype::str>(code, stack, offset);
}
void Arm64::i32store16(SHARED_PARAMS, uint64_t offset, uint64_t) {
    abstract_memop<memtype::h, resexttype::str>(code, stack, offset);
}
void Arm64::i64store8(SHARED_PARAMS, uint64_t offset, uint64_t) {
    abstract_memop<memtype::b, resexttype::str>(code, stack, offset);
}
void Arm64::i64store16(SHARED_PARAMS, uint64_t offset, uint64_t) {
    abstract_memop<memtype::h, resexttype::str>(code, stack, offset);
}
void Arm64::i64store32(SHARED_PARAMS, uint64_t offset, uint64_t) {
    abstract_memop<memtype::w, resexttype::str>(code, stack, offset);
}
void Arm64::i32eqz(SHARED_PARAMS) {
    auto [p] = allocate_registers<std::tuple<iwant::ireg>>(code);
    clobber_flags(code);

    raw::cmp(code, false, p.as<ireg>(), ireg::xzr);
    push(value::flag(cond::eq));

    finalize(code);
}
void Arm64::i64eqz(SHARED_PARAMS) {
    auto [p] = allocate_registers<std::tuple<iwant::ireg>>(code);
    clobber_flags(code);

    raw::cmp(code, true, p.as<ireg>(), ireg::xzr);
    push(value::flag(cond::eq));

    finalize(code);
}

#define COMPARISON(is_64, op)                                                  \
    do {                                                                       \
        auto [p1, p2] = allocate_registers<                                    \
            std::tuple<iwant::ireg, iwant::literal<1 << 12>>>(code);           \
        clobber_flags(code);                                                   \
        if (p2.is<value::location::imm>()) {                                   \
            raw::cmp(code, is_64, p1.as<ireg>(), p2.as<uint32_t>());           \
        } else {                                                               \
            raw::cmp(code, is_64, p1.as<ireg>(), p2.as<ireg>());               \
        }                                                                      \
        push(value::flag(cond::op));                                           \
        finalize(code);                                                        \
    } while (0)

void Arm64::i32eq(SHARED_PARAMS) { COMPARISON(false, eq); }
void Arm64::i64eq(SHARED_PARAMS) { COMPARISON(true, eq); }
void Arm64::i32ne(SHARED_PARAMS) { COMPARISON(false, ne); }
void Arm64::i64ne(SHARED_PARAMS) { COMPARISON(true, ne); }
void Arm64::i32lt_s(SHARED_PARAMS) { COMPARISON(false, lt); }
void Arm64::i64lt_s(SHARED_PARAMS) { COMPARISON(true, lt); }
void Arm64::i32lt_u(SHARED_PARAMS) { COMPARISON(false, cc); }
void Arm64::i64lt_u(SHARED_PARAMS) { COMPARISON(true, cc); }
void Arm64::i32gt_s(SHARED_PARAMS) { COMPARISON(false, gt); }
void Arm64::i64gt_s(SHARED_PARAMS) { COMPARISON(true, gt); }
void Arm64::i32gt_u(SHARED_PARAMS) { COMPARISON(false, hi); }
void Arm64::i64gt_u(SHARED_PARAMS) { COMPARISON(true, hi); }
void Arm64::i32le_s(SHARED_PARAMS) { COMPARISON(false, le); }
void Arm64::i64le_s(SHARED_PARAMS) { COMPARISON(true, le); }
void Arm64::i32le_u(SHARED_PARAMS) { COMPARISON(false, ls); }
void Arm64::i64le_u(SHARED_PARAMS) { COMPARISON(true, ls); }
void Arm64::i32ge_s(SHARED_PARAMS) { COMPARISON(false, ge); }
void Arm64::i64ge_s(SHARED_PARAMS) { COMPARISON(true, ge); }
void Arm64::i32ge_u(SHARED_PARAMS) { COMPARISON(false, cs); }
void Arm64::i64ge_u(SHARED_PARAMS) { COMPARISON(true, cs); }
#undef COMPARISON
#define COMPARISON(is_64, op)                                                  \
    do {                                                                       \
        auto [p1, p2] =                                                        \
            allocate_registers<std::tuple<iwant::freg, iwant::freg>>(code);    \
        clobber_flags(code);                                                   \
        raw::fcmp(code, is_64, p1.as<freg>(), p2.as<freg>());                  \
        push(value::flag(cond::op));                                           \
        finalize(code);                                                        \
    } while (0)

void Arm64::f32eq(SHARED_PARAMS) { COMPARISON(false, eq); }
void Arm64::f64eq(SHARED_PARAMS) { COMPARISON(true, eq); }
void Arm64::f32ne(SHARED_PARAMS) { COMPARISON(false, ne); }
void Arm64::f64ne(SHARED_PARAMS) { COMPARISON(true, ne); }
// todo: test if this can just be lt instead of mi
void Arm64::f32lt(SHARED_PARAMS) { COMPARISON(false, mi); }
void Arm64::f64lt(SHARED_PARAMS) { COMPARISON(true, mi); }
void Arm64::f32gt(SHARED_PARAMS) { COMPARISON(false, gt); }
void Arm64::f64gt(SHARED_PARAMS) { COMPARISON(true, gt); }
void Arm64::f32le(SHARED_PARAMS) { COMPARISON(false, ls); }
void Arm64::f64le(SHARED_PARAMS) { COMPARISON(true, ls); }
void Arm64::f32ge(SHARED_PARAMS) { COMPARISON(false, ge); }
void Arm64::f64ge(SHARED_PARAMS) { COMPARISON(true, ge); }
#undef COMPARISON

void Arm64::i32clz(SHARED_PARAMS) {
    auto [p1, res] =
        allocate_registers<std::tuple<iwant::ireg>, iwant::ireg>(code);

    raw::clz(code, false, p1.as<ireg>(), res.as<ireg>());

    finalize(code, res.as<ireg>());
}
void Arm64::i64clz(SHARED_PARAMS) {
    auto [p1, res] =
        allocate_registers<std::tuple<iwant::ireg>, iwant::ireg>(code);

    raw::clz(code, true, p1.as<ireg>(), res.as<ireg>());

    finalize(code, res.as<ireg>());
}
void Arm64::i32ctz(SHARED_PARAMS) {
    auto [p1, res] =
        allocate_registers<std::tuple<iwant::ireg>, iwant::ireg>(code);

    raw::rbit(code, false, p1.as<ireg>(), res.as<ireg>());
    raw::clz(code, false, res.as<ireg>(), res.as<ireg>());

    finalize(code, res.as<ireg>());
}
void Arm64::i64ctz(SHARED_PARAMS) {
    auto [p1, res] =
        allocate_registers<std::tuple<iwant::ireg>, iwant::ireg>(code);

    raw::rbit(code, true, p1.as<ireg>(), res.as<ireg>());
    raw::clz(code, true, res.as<ireg>(), res.as<ireg>());

    finalize(code, res.as<ireg>());
}
void Arm64::i32popcnt(SHARED_PARAMS) {
    auto [p1, res] =
        allocate_registers<std::tuple<iwant::ireg>, iwant::ireg>(code);

    auto s = floatregs.temporary();

    raw::mov(code, false, ftype::single, p1.as<ireg>(), s);
    raw::cnt(code, false, s, s);
    raw::addv(code, false, s, s);
    raw::mov(code, false, ftype::single, s, res.as<ireg>());

    finalize(code, res.as<ireg>());
}
void Arm64::i64popcnt(SHARED_PARAMS) {
    auto [p1, res] =
        allocate_registers<std::tuple<iwant::ireg>, iwant::ireg>(code);

    auto s = floatregs.temporary();

    raw::mov(code, true, ftype::double_, p1.as<ireg>(), s);
    raw::cnt(code, false, s, s);
    raw::addv(code, false, s, s);
    raw::mov(code, false, ftype::single, s, res.as<ireg>());

    finalize(code, res.as<ireg>());
}
void Arm64::i32add(SHARED_PARAMS) {
    auto [p1, p2, res] =
        allocate_registers<std::tuple<iwant::ireg, iwant::literal<>>,
                           iwant::ireg>(code);

    if (p2.is<value::location::imm>()) {
        masm::add(code, intregs, false, p2.as<uint32_t>(), p1.as<ireg>(),
                  res.as<ireg>());
    } else {
        raw::add(code, false, res.as<ireg>(), p1.as<ireg>(), p2.as<ireg>());
    }

    finalize(code, res.as<ireg>());
}
void Arm64::i64add(SHARED_PARAMS) {
    auto [p1, p2, res] =
        allocate_registers<std::tuple<iwant::ireg, iwant::literal<>>,
                           iwant::ireg>(code);

    if (p2.is<value::location::imm>()) {
        masm::add(code, intregs, true, p2.as<uint32_t>(), p1.as<ireg>(),
                  res.as<ireg>());
    } else {
        raw::add(code, true, res.as<ireg>(), p1.as<ireg>(), p2.as<ireg>());
    }

    finalize(code, res.as<ireg>());
}
void Arm64::i32sub(SHARED_PARAMS) {
    auto [p1, p2, res] =
        allocate_registers<std::tuple<iwant::ireg, iwant::literal<1 << 12>>,
                           iwant::ireg>(code);

    if (p2.is<value::location::imm>()) {
        raw::sub(code, false, res.as<ireg>(), p1.as<ireg>(), p2.as<uint32_t>());
    } else {
        raw::sub(code, false, res.as<ireg>(), p1.as<ireg>(), p2.as<ireg>());
    }

    finalize(code, res.as<ireg>());
}
void Arm64::i64sub(SHARED_PARAMS) {
    auto [p1, p2, res] =
        allocate_registers<std::tuple<iwant::ireg, iwant::literal<1 << 12>>,
                           iwant::ireg>(code);

    if (p2.is<value::location::imm>()) {
        raw::sub(code, true, res.as<ireg>(), p1.as<ireg>(), p2.as<uint32_t>());
    } else {
        raw::sub(code, true, res.as<ireg>(), p1.as<ireg>(), p2.as<ireg>());
    }

    finalize(code, res.as<ireg>());
}
void Arm64::i32mul(SHARED_PARAMS) {
    auto [p1, p2, res] =
        allocate_registers<std::tuple<iwant::ireg, iwant::ireg>, iwant::ireg>(
            code);
    raw::mul(code, false, p2.as<ireg>(), p1.as<ireg>(), res.as<ireg>());
    finalize(code, res.as<ireg>());
}
void Arm64::i64mul(SHARED_PARAMS) {
    auto [p1, p2, res] =
        allocate_registers<std::tuple<iwant::ireg, iwant::ireg>, iwant::ireg>(
            code);
    raw::mul(code, true, p2.as<ireg>(), p1.as<ireg>(), res.as<ireg>());
    finalize(code, res.as<ireg>());
}
void Arm64::i32div_s(SHARED_PARAMS) {
    auto [p1, p2, res] =
        allocate_registers<std::tuple<iwant::ireg, iwant::ireg>, iwant::ireg>(
            code);

    clobber_flags(code);

    auto zero_check = code;
    raw::cbnz(code, false, 0, p2.as<ireg>());
    auto [_, overflow_trap] = masm::trap(
        code, std::to_array({runtime::TrapKind::integer_divide_by_zero,
                             runtime::TrapKind::integer_overflow}));
    amend_br_if(zero_check, code);
    raw::cmn(code, false, p2.as<ireg>(), 1);
    raw::ccmp(code, false, 1, cond::eq, p1.as<ireg>(), 0);
    raw::bcond(code, overflow_trap - code, cond::vs);

    raw::sdiv(code, false, p2.as<ireg>(), p1.as<ireg>(), res.as<ireg>());
    finalize(code, res.as<ireg>());
}
void Arm64::i64div_s(SHARED_PARAMS) {
    auto [p1, p2, res] =
        allocate_registers<std::tuple<iwant::ireg, iwant::ireg>, iwant::ireg>(
            code);

    clobber_flags(code);

    auto zero_check = code;
    raw::cbnz(code, true, 0, p2.as<ireg>());
    auto [_, overflow_trap] = masm::trap(
        code, std::to_array({runtime::TrapKind::integer_divide_by_zero,
                             runtime::TrapKind::integer_overflow}));
    amend_br_if(zero_check, code);
    raw::cmn(code, true, p2.as<ireg>(), 1);
    raw::ccmp(code, true, 1, cond::eq, p1.as<ireg>(), 0);
    raw::bcond(code, overflow_trap - code, cond::vs);

    raw::sdiv(code, true, p2.as<ireg>(), p1.as<ireg>(), res.as<ireg>());
    finalize(code, res.as<ireg>());
}
void Arm64::i32div_u(SHARED_PARAMS) {
    auto [p1, p2, res] =
        allocate_registers<std::tuple<iwant::ireg, iwant::ireg>, iwant::ireg>(
            code);

    clobber_flags(code);

    auto zero_check = code;
    raw::cbnz(code, false, 0, p2.as<ireg>());
    masm::trap(code, runtime::TrapKind::integer_divide_by_zero);
    amend_br_if(zero_check, code);

    raw::udiv(code, false, p2.as<ireg>(), p1.as<ireg>(), res.as<ireg>());
    finalize(code, res.as<ireg>());
}
void Arm64::i64div_u(SHARED_PARAMS) {
    auto [p1, p2, res] =
        allocate_registers<std::tuple<iwant::ireg, iwant::ireg>, iwant::ireg>(
            code);

    clobber_flags(code);

    auto zero_check = code;
    raw::cbnz(code, true, 0, p2.as<ireg>());
    masm::trap(code, runtime::TrapKind::integer_divide_by_zero);
    amend_br_if(zero_check, code);

    raw::udiv(code, true, p2.as<ireg>(), p1.as<ireg>(), res.as<ireg>());
    finalize(code, res.as<ireg>());
}
void Arm64::i32rem_s(SHARED_PARAMS) {
    auto [p1, p2, res] =
        allocate_registers<std::tuple<iwant::ireg, iwant::ireg>, iwant::ireg>(
            code);
    auto v = intregs.temporary();
    clobber_flags(code);

    auto zero_check = code;
    raw::cbnz(code, false, 0, p2.as<ireg>());
    masm::trap(code, runtime::TrapKind::integer_divide_by_zero);
    amend_br_if(zero_check, code);

    raw::sdiv(code, false, p2.as<ireg>(), p1.as<ireg>(), v);
    raw::msub(code, false, p2.as<ireg>(), p1.as<ireg>(), v, res.as<ireg>());
    finalize(code, res.as<ireg>());
}
void Arm64::i64rem_s(SHARED_PARAMS) {
    auto [p1, p2, res] =
        allocate_registers<std::tuple<iwant::ireg, iwant::ireg>, iwant::ireg>(
            code);
    auto v = intregs.temporary();
    clobber_flags(code);

    auto zero_check = code;
    raw::cbnz(code, true, 0, p2.as<ireg>());
    masm::trap(code, runtime::TrapKind::integer_divide_by_zero);
    amend_br_if(zero_check, code);

    raw::sdiv(code, true, p2.as<ireg>(), p1.as<ireg>(), v);
    raw::msub(code, true, p2.as<ireg>(), p1.as<ireg>(), v, res.as<ireg>());
    finalize(code, res.as<ireg>());
}
void Arm64::i32rem_u(SHARED_PARAMS) {
    auto [p1, p2, res] =
        allocate_registers<std::tuple<iwant::ireg, iwant::ireg>, iwant::ireg>(
            code);
    auto v = intregs.temporary();
    clobber_flags(code);

    auto zero_check = code;
    raw::cbnz(code, false, 0, p2.as<ireg>());
    masm::trap(code, runtime::TrapKind::integer_divide_by_zero);
    amend_br_if(zero_check, code);

    raw::udiv(code, false, p2.as<ireg>(), p1.as<ireg>(), v);
    raw::msub(code, false, p2.as<ireg>(), p1.as<ireg>(), v, res.as<ireg>());
    finalize(code, res.as<ireg>());
}
void Arm64::i64rem_u(SHARED_PARAMS) {
    auto [p1, p2, res] =
        allocate_registers<std::tuple<iwant::ireg, iwant::ireg>, iwant::ireg>(
            code);
    auto v = intregs.temporary();
    clobber_flags(code);

    auto zero_check = code;
    raw::cbnz(code, true, 0, p2.as<ireg>());
    masm::trap(code, runtime::TrapKind::integer_divide_by_zero);
    amend_br_if(zero_check, code);

    raw::udiv(code, true, p2.as<ireg>(), p1.as<ireg>(), v);
    raw::msub(code, true, p2.as<ireg>(), p1.as<ireg>(), v, res.as<ireg>());
    finalize(code, res.as<ireg>());
}
void Arm64::i32and(SHARED_PARAMS) {
    auto [p1, p2, res] =
        allocate_registers<std::tuple<iwant::ireg, iwant::bitmask<uint32_t>>,
                           iwant::ireg>(code);

    if (p2.is<value::location::imm>()) {
        raw::and_(code, false, p2.as<LogicalImm>(), p1.as<ireg>(),
                  res.as<ireg>());
    } else {
        raw::and_(code, false, shifttype::lsl, p2.as<ireg>(), 0, p1.as<ireg>(),
                  res.as<ireg>());
    }

    finalize(code, res.as<ireg>());
}
void Arm64::i64and(SHARED_PARAMS) {
    auto [p1, p2, res] =
        allocate_registers<std::tuple<iwant::ireg, iwant::bitmask<uint64_t>>,
                           iwant::ireg>(code);

    if (p2.is<value::location::imm>()) {
        raw::and_(code, true, p2.as<LogicalImm>(), p1.as<ireg>(),
                  res.as<ireg>());
    } else {
        raw::and_(code, true, shifttype::lsl, p2.as<ireg>(), 0, p1.as<ireg>(),
                  res.as<ireg>());
    }

    finalize(code, res.as<ireg>());
}
void Arm64::i32or(SHARED_PARAMS) {
    auto [p1, p2, res] =
        allocate_registers<std::tuple<iwant::ireg, iwant::bitmask<uint32_t>>,
                           iwant::ireg>(code);

    if (p2.is<value::location::imm>()) {
        raw::orr(code, false, p2.as<LogicalImm>(), p1.as<ireg>(),
                 res.as<ireg>());
    } else {
        raw::orr(code, false, shifttype::lsl, p2.as<ireg>(), 0, p1.as<ireg>(),
                 res.as<ireg>());
    }

    finalize(code, res.as<ireg>());
}
void Arm64::i64or(SHARED_PARAMS) {
    auto [p1, p2, res] =
        allocate_registers<std::tuple<iwant::ireg, iwant::bitmask<uint64_t>>,
                           iwant::ireg>(code);

    if (p2.is<value::location::imm>()) {
        raw::orr(code, true, p2.as<LogicalImm>(), p1.as<ireg>(),
                 res.as<ireg>());
    } else {
        raw::orr(code, true, shifttype::lsl, p2.as<ireg>(), 0, p1.as<ireg>(),
                 res.as<ireg>());
    }

    finalize(code, res.as<ireg>());
}
void Arm64::i32xor(SHARED_PARAMS) {
    auto [p1, p2, res] =
        allocate_registers<std::tuple<iwant::ireg, iwant::bitmask<uint32_t>>,
                           iwant::ireg>(code);

    if (p2.is<value::location::imm>()) {
        raw::eor(code, false, p2.as<LogicalImm>(), p1.as<ireg>(),
                 res.as<ireg>());
    } else {
        raw::eor(code, false, shifttype::lsl, p2.as<ireg>(), 0, p1.as<ireg>(),
                 res.as<ireg>());
    }

    finalize(code, res.as<ireg>());
}
void Arm64::i64xor(SHARED_PARAMS) {
    auto [p1, p2, res] =
        allocate_registers<std::tuple<iwant::ireg, iwant::bitmask<uint64_t>>,
                           iwant::ireg>(code);

    if (p2.is<value::location::imm>()) {
        raw::eor(code, true, p2.as<LogicalImm>(), p1.as<ireg>(),
                 res.as<ireg>());
    } else {
        raw::eor(code, true, shifttype::lsl, p2.as<ireg>(), 0, p1.as<ireg>(),
                 res.as<ireg>());
    }

    finalize(code, res.as<ireg>());
}
void Arm64::i32shl(SHARED_PARAMS) {
    auto [p1, p2, res] =
        allocate_registers<std::tuple<iwant::ireg, iwant::literal<>>,
                           iwant::ireg>(code);

    if (p2.is<value::location::imm>()) {
        raw::lsl(code, false, p2.as<uint32_t>(), p1.as<ireg>(), res.as<ireg>());
    } else {
        raw::lsl(code, false, p2.as<ireg>(), p1.as<ireg>(), res.as<ireg>());
    }

    finalize(code, res.as<ireg>());
}
void Arm64::i64shl(SHARED_PARAMS) {
    auto [p1, p2, res] =
        allocate_registers<std::tuple<iwant::ireg, iwant::literal<>>,
                           iwant::ireg>(code);

    if (p2.is<value::location::imm>()) {
        raw::lsl(code, true, p2.as<uint32_t>(), p1.as<ireg>(), res.as<ireg>());
    } else {
        raw::lsl(code, true, p2.as<ireg>(), p1.as<ireg>(), res.as<ireg>());
    }

    finalize(code, res.as<ireg>());
}
void Arm64::i32shr_s(SHARED_PARAMS) {
    auto [p1, p2, res] =
        allocate_registers<std::tuple<iwant::ireg, iwant::literal<>>,
                           iwant::ireg>(code);

    if (p2.is<value::location::imm>()) {
        raw::asr(code, false, p2.as<uint32_t>(), p1.as<ireg>(), res.as<ireg>());
    } else {
        raw::asr(code, false, p2.as<ireg>(), p1.as<ireg>(), res.as<ireg>());
    }

    finalize(code, res.as<ireg>());
}
void Arm64::i64shr_s(SHARED_PARAMS) {
    auto [p1, p2, res] =
        allocate_registers<std::tuple<iwant::ireg, iwant::literal<>>,
                           iwant::ireg>(code);

    if (p2.is<value::location::imm>()) {
        raw::asr(code, true, p2.as<uint32_t>(), p1.as<ireg>(), res.as<ireg>());
    } else {
        raw::asr(code, true, p2.as<ireg>(), p1.as<ireg>(), res.as<ireg>());
    }

    finalize(code, res.as<ireg>());
}
void Arm64::i32shr_u(SHARED_PARAMS) {
    auto [p1, p2, res] =
        allocate_registers<std::tuple<iwant::ireg, iwant::literal<>>,
                           iwant::ireg>(code);

    if (p2.is<value::location::imm>()) {
        raw::lsr(code, false, p2.as<uint32_t>(), p1.as<ireg>(), res.as<ireg>());
    } else {
        raw::lsr(code, false, p2.as<ireg>(), p1.as<ireg>(), res.as<ireg>());
    }

    finalize(code, res.as<ireg>());
}
void Arm64::i64shr_u(SHARED_PARAMS) {
    auto [p1, p2, res] =
        allocate_registers<std::tuple<iwant::ireg, iwant::literal<>>,
                           iwant::ireg>(code);

    if (p2.is<value::location::imm>()) {
        raw::lsr(code, true, p2.as<uint32_t>(), p1.as<ireg>(), res.as<ireg>());
    } else {
        raw::lsr(code, true, p2.as<ireg>(), p1.as<ireg>(), res.as<ireg>());
    }

    finalize(code, res.as<ireg>());
}
void Arm64::i32rotl(SHARED_PARAMS) {
    auto [p1, p2, res] =
        allocate_registers<std::tuple<iwant::ireg, iwant::literal<>>,
                           iwant::ireg>(code);

    if (p2.is<value::location::imm>()) {
        raw::ror(code, false, 32 - (p2.as<uint32_t>() & 31), p1.as<ireg>(),
                 res.as<ireg>());
    } else {
        auto temp = intregs.temporary();
        raw::neg(code, false, p2.as<ireg>(), temp);
        raw::ror(code, false, temp, p1.as<ireg>(), res.as<ireg>());
    }

    finalize(code, res.as<ireg>());
}
void Arm64::i64rotl(SHARED_PARAMS) {
    auto [p1, p2, res] =
        allocate_registers<std::tuple<iwant::ireg, iwant::literal<>>,
                           iwant::ireg>(code);

    if (p2.is<value::location::imm>()) {
        raw::ror(code, true, 64 - (p2.as<uint32_t>() & 63), p1.as<ireg>(),
                 res.as<ireg>());
    } else {
        auto temp = intregs.temporary();
        raw::neg(code, true, p2.as<ireg>(), temp);
        raw::ror(code, true, temp, p1.as<ireg>(), res.as<ireg>());
    }

    finalize(code, res.as<ireg>());
}
void Arm64::i32rotr(SHARED_PARAMS) {
    auto [p1, p2, res] =
        allocate_registers<std::tuple<iwant::ireg, iwant::literal<>>,
                           iwant::ireg>(code);

    if (p2.is<value::location::imm>()) {
        raw::ror(code, false, p2.as<uint32_t>(), p1.as<ireg>(), res.as<ireg>());
    } else {
        raw::ror(code, false, p2.as<ireg>(), p1.as<ireg>(), res.as<ireg>());
    }

    finalize(code, res.as<ireg>());
}
void Arm64::i64rotr(SHARED_PARAMS) {
    auto [p1, p2, res] =
        allocate_registers<std::tuple<iwant::ireg, iwant::literal<>>,
                           iwant::ireg>(code);

    if (p2.is<value::location::imm>()) {
        raw::ror(code, true, p2.as<uint32_t>(), p1.as<ireg>(), res.as<ireg>());
    } else {
        raw::ror(code, true, p2.as<ireg>(), p1.as<ireg>(), res.as<ireg>());
    }

    finalize(code, res.as<ireg>());
}
void Arm64::f32abs(SHARED_PARAMS) {
    auto [p1, res] =
        allocate_registers<std::tuple<iwant::freg>, iwant::freg>(code);
    raw::fabs(code, ftype::single, p1.as<freg>(), res.as<freg>());
    finalize(code, res.as<freg>());
}
void Arm64::f64abs(SHARED_PARAMS) {
    auto [p1, res] =
        allocate_registers<std::tuple<iwant::freg>, iwant::freg>(code);
    raw::fabs(code, ftype::double_, p1.as<freg>(), res.as<freg>());
    finalize(code, res.as<freg>());
}
void Arm64::f32neg(SHARED_PARAMS) {
    auto [p1, res] =
        allocate_registers<std::tuple<iwant::freg>, iwant::freg>(code);
    raw::fneg(code, ftype::single, p1.as<freg>(), res.as<freg>());
    finalize(code, res.as<freg>());
}
void Arm64::f64neg(SHARED_PARAMS) {
    auto [p1, res] =
        allocate_registers<std::tuple<iwant::freg>, iwant::freg>(code);
    raw::fneg(code, ftype::double_, p1.as<freg>(), res.as<freg>());
    finalize(code, res.as<freg>());
}
void Arm64::f32ceil(SHARED_PARAMS) {
    auto [p1, res] =
        allocate_registers<std::tuple<iwant::freg>, iwant::freg>(code);
    raw::frintp(code, ftype::single, p1.as<freg>(), res.as<freg>());
    finalize(code, res.as<freg>());
}
void Arm64::f64ceil(SHARED_PARAMS) {
    auto [p1, res] =
        allocate_registers<std::tuple<iwant::freg>, iwant::freg>(code);
    raw::frintp(code, ftype::double_, p1.as<freg>(), res.as<freg>());
    finalize(code, res.as<freg>());
}
void Arm64::f32floor(SHARED_PARAMS) {
    auto [p1, res] =
        allocate_registers<std::tuple<iwant::freg>, iwant::freg>(code);
    raw::frintm(code, ftype::single, p1.as<freg>(), res.as<freg>());
    finalize(code, res.as<freg>());
}
void Arm64::f64floor(SHARED_PARAMS) {
    auto [p1, res] =
        allocate_registers<std::tuple<iwant::freg>, iwant::freg>(code);
    raw::frintm(code, ftype::double_, p1.as<freg>(), res.as<freg>());
    finalize(code, res.as<freg>());
}
void Arm64::f32trunc(SHARED_PARAMS) {
    auto [p1, res] =
        allocate_registers<std::tuple<iwant::freg>, iwant::freg>(code);
    raw::frintz(code, ftype::single, p1.as<freg>(), res.as<freg>());
    finalize(code, res.as<freg>());
}
void Arm64::f64trunc(SHARED_PARAMS) {
    auto [p1, res] =
        allocate_registers<std::tuple<iwant::freg>, iwant::freg>(code);
    raw::frintz(code, ftype::double_, p1.as<freg>(), res.as<freg>());
    finalize(code, res.as<freg>());
}
void Arm64::f32nearest(SHARED_PARAMS) {
    auto [p1, res] =
        allocate_registers<std::tuple<iwant::freg>, iwant::freg>(code);
    raw::frinti(code, ftype::single, p1.as<freg>(), res.as<freg>());
    finalize(code, res.as<freg>());
}
void Arm64::f64nearest(SHARED_PARAMS) {
    auto [p1, res] =
        allocate_registers<std::tuple<iwant::freg>, iwant::freg>(code);
    raw::frinti(code, ftype::double_, p1.as<freg>(), res.as<freg>());
    finalize(code, res.as<freg>());
}
void Arm64::f32sqrt(SHARED_PARAMS) {
    auto [p1, res] =
        allocate_registers<std::tuple<iwant::freg>, iwant::freg>(code);
    raw::fsqrt(code, ftype::single, p1.as<freg>(), res.as<freg>());
    finalize(code, res.as<freg>());
}
void Arm64::f64sqrt(SHARED_PARAMS) {
    auto [p1, res] =
        allocate_registers<std::tuple<iwant::freg>, iwant::freg>(code);
    raw::fsqrt(code, ftype::double_, p1.as<freg>(), res.as<freg>());
    finalize(code, res.as<freg>());
}
void Arm64::f32add(SHARED_PARAMS) {
    auto [p1, p2, res] =
        allocate_registers<std::tuple<iwant::freg, iwant::freg>, iwant::freg>(
            code);
    raw::fadd(code, ftype::single, p2.as<freg>(), p1.as<freg>(),
              res.as<freg>());
    finalize(code, res.as<freg>());
}
void Arm64::f64add(SHARED_PARAMS) {
    auto [p1, p2, res] =
        allocate_registers<std::tuple<iwant::freg, iwant::freg>, iwant::freg>(
            code);
    raw::fadd(code, ftype::double_, p2.as<freg>(), p1.as<freg>(),
              res.as<freg>());
    finalize(code, res.as<freg>());
}
void Arm64::f32sub(SHARED_PARAMS) {
    auto [p1, p2, res] =
        allocate_registers<std::tuple<iwant::freg, iwant::freg>, iwant::freg>(
            code);
    raw::fsub(code, ftype::single, p2.as<freg>(), p1.as<freg>(),
              res.as<freg>());
    finalize(code, res.as<freg>());
}
void Arm64::f64sub(SHARED_PARAMS) {
    auto [p1, p2, res] =
        allocate_registers<std::tuple<iwant::freg, iwant::freg>, iwant::freg>(
            code);
    raw::fsub(code, ftype::double_, p2.as<freg>(), p1.as<freg>(),
              res.as<freg>());
    finalize(code, res.as<freg>());
}
void Arm64::f32mul(SHARED_PARAMS) {
    auto [p1, p2, res] =
        allocate_registers<std::tuple<iwant::freg, iwant::freg>, iwant::freg>(
            code);
    raw::fmul(code, ftype::single, p2.as<freg>(), p1.as<freg>(),
              res.as<freg>());
    finalize(code, res.as<freg>());
}
void Arm64::f64mul(SHARED_PARAMS) {
    auto [p1, p2, res] =
        allocate_registers<std::tuple<iwant::freg, iwant::freg>, iwant::freg>(
            code);
    raw::fmul(code, ftype::double_, p2.as<freg>(), p1.as<freg>(),
              res.as<freg>());
    finalize(code, res.as<freg>());
}
void Arm64::f32div(SHARED_PARAMS) {
    auto [p1, p2, res] =
        allocate_registers<std::tuple<iwant::freg, iwant::freg>, iwant::freg>(
            code);
    raw::fdiv(code, ftype::single, p2.as<freg>(), p1.as<freg>(),
              res.as<freg>());
    finalize(code, res.as<freg>());
}
void Arm64::f64div(SHARED_PARAMS) {
    auto [p1, p2, res] =
        allocate_registers<std::tuple<iwant::freg, iwant::freg>, iwant::freg>(
            code);
    raw::fdiv(code, ftype::double_, p2.as<freg>(), p1.as<freg>(),
              res.as<freg>());
    finalize(code, res.as<freg>());
}
void Arm64::f32min(SHARED_PARAMS) {
    auto [p1, p2, res] =
        allocate_registers<std::tuple<iwant::freg, iwant::freg>, iwant::freg>(
            code);
    raw::fmin(code, ftype::single, p2.as<freg>(), p1.as<freg>(),
              res.as<freg>());
    finalize(code, res.as<freg>());
}
void Arm64::f64min(SHARED_PARAMS) {
    auto [p1, p2, res] =
        allocate_registers<std::tuple<iwant::freg, iwant::freg>, iwant::freg>(
            code);
    raw::fmin(code, ftype::double_, p2.as<freg>(), p1.as<freg>(),
              res.as<freg>());
    finalize(code, res.as<freg>());
}
void Arm64::f32max(SHARED_PARAMS) {
    auto [p1, p2, res] =
        allocate_registers<std::tuple<iwant::freg, iwant::freg>, iwant::freg>(
            code);
    raw::fmax(code, ftype::single, p2.as<freg>(), p1.as<freg>(),
              res.as<freg>());
    finalize(code, res.as<freg>());
}
void Arm64::f64max(SHARED_PARAMS) {
    auto [p1, p2, res] =
        allocate_registers<std::tuple<iwant::freg, iwant::freg>, iwant::freg>(
            code);
    raw::fmax(code, ftype::double_, p2.as<freg>(), p1.as<freg>(),
              res.as<freg>());
    finalize(code, res.as<freg>());
}
void Arm64::f32copysign(SHARED_PARAMS) {
    auto [p1, p2, res] =
        allocate_registers<std::tuple<iwant::freg, iwant::freg>, iwant::freg>(
            code);
    auto i1 = intregs.temporary(), i2 = intregs.temporary();

    raw::mov(code, false, ftype::single, p1.as<freg>(), i1);
    raw::mov(code, false, ftype::single, p2.as<freg>(), i2);
    raw::and_(code, false, *tryLogicalImm(0x80000000), i2, i2);
    raw::orr(code, false, shifttype::lsl, i2, 0, i1, i1);
    raw::mov(code, false, ftype::single, i1, res.as<freg>());

    finalize(code, res.as<freg>());
}
void Arm64::f64copysign(SHARED_PARAMS) {
    auto [p1, p2, res] =
        allocate_registers<std::tuple<iwant::freg, iwant::freg>, iwant::freg>(
            code);
    auto i1 = intregs.temporary(), i2 = intregs.temporary();

    raw::mov(code, true, ftype::double_, p1.as<freg>(), i1);
    raw::mov(code, true, ftype::double_, p2.as<freg>(), i2);
    raw::and_(code, true, *tryLogicalImm(0x8000000000000000ull), i2, i2);
    raw::orr(code, true, shifttype::lsl, i2, 0, i1, i1);
    raw::mov(code, true, ftype::double_, i1, res.as<freg>());

    finalize(code, res.as<freg>());
}
// todo: this should probably be a noop?
// the fact it isn't implies there's issues elsewhere
void Arm64::i32wrap_i64(SHARED_PARAMS) {
    auto [p1, res] =
        allocate_registers<std::tuple<iwant::ireg>, iwant::ireg>(code);

    raw::mov(code, false, p1.as<ireg>(), res.as<ireg>());

    finalize(code, res.as<ireg>());
}
void Arm64::i64extend_i32_s(SHARED_PARAMS) {
    auto [p1, res] =
        allocate_registers<std::tuple<iwant::ireg>, iwant::ireg>(code);

    raw::sxtw(code, p1.as<ireg>(), res.as<ireg>());

    finalize(code, res.as<ireg>());
}
void Arm64::i64extend_i32_u(SHARED_PARAMS) {
    auto [p1, res] =
        allocate_registers<std::tuple<iwant::ireg>, iwant::ireg>(code);

    raw::mov(code, false, p1.as<ireg>(), res.as<ireg>());

    finalize(code, res.as<ireg>());
}

template <typename FloatType>
void Arm64::validate_trunc(std::byte *&code, freg v, FloatType lower,
                           FloatType upper) {
    using IntType = std::conditional_t<std::is_same_v<FloatType, float>,
                                       uint32_t, uint64_t>;
    constexpr auto ft =
        std::is_same_v<FloatType, float> ? ftype::single : ftype::double_;
    constexpr auto sf = ft == ftype::double_;
    constexpr auto nonfinite_value = ft == ftype::single
                                         ? (IntType)0x7F80'0000
                                         : (IntType)0x7FF0'0000'0000'0000;
    constexpr auto signless_bits = ft == ftype::single
                                       ? (IntType)0x7FFF'FFFF
                                       : (IntType)0x7FFF'FFFF'FFFF'FFFF;
    static_assert(tryLogicalImm(signless_bits) != std::nullopt);

    clobber_flags(code);

    auto int_bits = intregs.temporary(), int_comparison = intregs.temporary();
    auto float_comparison = floatregs.temporary();

    raw::mov(code, sf, ft, v, int_bits);
    masm::mov(code, nonfinite_value, int_comparison);
    raw::and_(code, sf, *tryLogicalImm(signless_bits), int_bits, int_bits);
    raw::cmp(code, sf, int_bits, int_comparison);

    auto isfinite_check = code;
    raw::bcond(code, 0, cond::lt);
    raw::fcmp(code, sf, v, v);

    auto nan_check = code;
    raw::bcond(code, 0, cond::eq);
    auto [_, overflow_trap] = masm::trap(
        code, std::to_array({runtime::TrapKind::invalid_conversion_to_integer,
                             runtime::TrapKind::integer_overflow}));
    amend_br_if(nan_check, overflow_trap);
    amend_br_if(isfinite_check, code);

    masm::mov(code, std::bit_cast<IntType>(lower), int_comparison);
    raw::mov(code, sf, ft, int_comparison, float_comparison);
    raw::fcmp(code, sf, v, float_comparison);
    raw::bcond(code, overflow_trap - code, cond::ls);

    masm::mov(code, std::bit_cast<IntType>(upper), int_comparison);
    raw::mov(code, sf, ft, int_comparison, float_comparison);
    raw::fcmp(code, sf, v, float_comparison);
    raw::bcond(code, overflow_trap - code, cond::ge);
}

void Arm64::i32trunc_f32_s(SHARED_PARAMS) {
    auto [p1, res] =
        allocate_registers<std::tuple<iwant::freg>, iwant::ireg>(code);

    validate_trunc(code, p1.as<freg>(), -2147483904.f, 2147483647.f);
    raw::fcvtzs(code, false, ftype::single, p1.as<freg>(), res.as<ireg>());

    finalize(code, res.as<ireg>());
}
void Arm64::i64trunc_f32_s(SHARED_PARAMS) {
    auto [p1, res] =
        allocate_registers<std::tuple<iwant::freg>, iwant::ireg>(code);

    validate_trunc(code, p1.as<freg>(), -9223373136366404000.f,
                   9223372036854776000.f);
    raw::fcvtzs(code, true, ftype::single, p1.as<freg>(), res.as<ireg>());

    finalize(code, res.as<ireg>());
}
void Arm64::i32trunc_f32_u(SHARED_PARAMS) {
    auto [p1, res] =
        allocate_registers<std::tuple<iwant::freg>, iwant::ireg>(code);

    validate_trunc(code, p1.as<freg>(), -1.f, 4294967296.f);
    raw::fcvtzu(code, false, ftype::single, p1.as<freg>(), res.as<ireg>());

    finalize(code, res.as<ireg>());
}
void Arm64::i64trunc_f32_u(SHARED_PARAMS) {
    auto [p1, res] =
        allocate_registers<std::tuple<iwant::freg>, iwant::ireg>(code);

    validate_trunc(code, p1.as<freg>(), -1.f, 18446744073709552000.f);
    raw::fcvtzu(code, true, ftype::single, p1.as<freg>(), res.as<ireg>());

    finalize(code, res.as<ireg>());
}
void Arm64::i32trunc_f64_s(SHARED_PARAMS) {
    auto [p1, res] =
        allocate_registers<std::tuple<iwant::freg>, iwant::ireg>(code);

    validate_trunc(code, p1.as<freg>(), -2147483649., 2147483648.);
    raw::fcvtzs(code, false, ftype::double_, p1.as<freg>(), res.as<ireg>());

    finalize(code, res.as<ireg>());
}
void Arm64::i64trunc_f64_s(SHARED_PARAMS) {
    auto [p1, res] =
        allocate_registers<std::tuple<iwant::freg>, iwant::ireg>(code);

    validate_trunc(code, p1.as<freg>(), -9223372036854777856.,
                   9223372036854776000.);
    raw::fcvtzs(code, true, ftype::double_, p1.as<freg>(), res.as<ireg>());

    finalize(code, res.as<ireg>());
}
void Arm64::i32trunc_f64_u(SHARED_PARAMS) {
    auto [p1, res] =
        allocate_registers<std::tuple<iwant::freg>, iwant::ireg>(code);

    validate_trunc(code, p1.as<freg>(), -1., 4294967296.);
    raw::fcvtzu(code, false, ftype::double_, p1.as<freg>(), res.as<ireg>());

    finalize(code, res.as<ireg>());
}
void Arm64::i64trunc_f64_u(SHARED_PARAMS) {
    auto [p1, res] =
        allocate_registers<std::tuple<iwant::freg>, iwant::ireg>(code);

    validate_trunc(code, p1.as<freg>(), -1., 18446744073709552000.);
    raw::fcvtzu(code, true, ftype::double_, p1.as<freg>(), res.as<ireg>());

    finalize(code, res.as<ireg>());
}
void Arm64::f32convert_i32_s(SHARED_PARAMS) {
    auto [p1, res] =
        allocate_registers<std::tuple<iwant::ireg>, iwant::freg>(code);
    raw::scvtf(code, false, ftype::single, p1.as<ireg>(), res.as<freg>());
    finalize(code, res.as<freg>());
}
void Arm64::f64convert_i32_s(SHARED_PARAMS) {
    auto [p1, res] =
        allocate_registers<std::tuple<iwant::ireg>, iwant::freg>(code);
    raw::scvtf(code, false, ftype::double_, p1.as<ireg>(), res.as<freg>());
    finalize(code, res.as<freg>());
}
void Arm64::f32convert_i32_u(SHARED_PARAMS) {
    auto [p1, res] =
        allocate_registers<std::tuple<iwant::ireg>, iwant::freg>(code);
    raw::ucvtf(code, false, ftype::single, p1.as<ireg>(), res.as<freg>());
    finalize(code, res.as<freg>());
}
void Arm64::f64convert_i32_u(SHARED_PARAMS) {
    auto [p1, res] =
        allocate_registers<std::tuple<iwant::ireg>, iwant::freg>(code);
    raw::ucvtf(code, false, ftype::double_, p1.as<ireg>(), res.as<freg>());
    finalize(code, res.as<freg>());
}
void Arm64::f32convert_i64_s(SHARED_PARAMS) {
    auto [p1, res] =
        allocate_registers<std::tuple<iwant::ireg>, iwant::freg>(code);
    raw::scvtf(code, true, ftype::single, p1.as<ireg>(), res.as<freg>());
    finalize(code, res.as<freg>());
}
void Arm64::f64convert_i64_s(SHARED_PARAMS) {
    auto [p1, res] =
        allocate_registers<std::tuple<iwant::ireg>, iwant::freg>(code);
    raw::scvtf(code, true, ftype::double_, p1.as<ireg>(), res.as<freg>());
    finalize(code, res.as<freg>());
}
void Arm64::f32convert_i64_u(SHARED_PARAMS) {
    auto [p1, res] =
        allocate_registers<std::tuple<iwant::ireg>, iwant::freg>(code);
    raw::ucvtf(code, true, ftype::single, p1.as<ireg>(), res.as<freg>());
    finalize(code, res.as<freg>());
}
void Arm64::f64convert_i64_u(SHARED_PARAMS) {
    auto [p1, res] =
        allocate_registers<std::tuple<iwant::ireg>, iwant::freg>(code);
    raw::ucvtf(code, true, ftype::double_, p1.as<ireg>(), res.as<freg>());
    finalize(code, res.as<freg>());
}
void Arm64::f32demote_f64(SHARED_PARAMS) {
    auto [p1, res] =
        allocate_registers<std::tuple<iwant::freg>, iwant::freg>(code);
    raw::fcvt(code, ftype::double_, ftype::single, p1.as<freg>(),
              res.as<freg>());
    finalize(code, res.as<freg>());
}
void Arm64::f64promote_f32(SHARED_PARAMS) {
    auto [p1, res] =
        allocate_registers<std::tuple<iwant::freg>, iwant::freg>(code);
    raw::fcvt(code, ftype::single, ftype::double_, p1.as<freg>(),
              res.as<freg>());
    finalize(code, res.as<freg>());
}
void Arm64::i32reinterpret_f32(SHARED_PARAMS) {
    auto [p1, res] =
        allocate_registers<std::tuple<iwant::freg>, iwant::ireg>(code);
    raw::mov(code, false, ftype::single, p1.as<freg>(), res.as<ireg>());
    finalize(code, res.as<ireg>());
}
void Arm64::f32reinterpret_i32(SHARED_PARAMS) {
    auto [p1, res] =
        allocate_registers<std::tuple<iwant::ireg>, iwant::freg>(code);
    raw::mov(code, false, ftype::single, p1.as<ireg>(), res.as<freg>());
    finalize(code, res.as<freg>());
}
void Arm64::i64reinterpret_f64(SHARED_PARAMS) {
    auto [p1, res] =
        allocate_registers<std::tuple<iwant::freg>, iwant::ireg>(code);
    raw::mov(code, true, ftype::double_, p1.as<freg>(), res.as<ireg>());
    finalize(code, res.as<ireg>());
}
void Arm64::f64reinterpret_i64(SHARED_PARAMS) {
    auto [p1, res] =
        allocate_registers<std::tuple<iwant::ireg>, iwant::freg>(code);
    raw::mov(code, true, ftype::double_, p1.as<ireg>(), res.as<freg>());
    finalize(code, res.as<freg>());
}
void Arm64::i32extend8_s(SHARED_PARAMS) {
    auto [p1, res] =
        allocate_registers<std::tuple<iwant::ireg>, iwant::ireg>(code);

    raw::sxtb(code, false, p1.as<ireg>(), res.as<ireg>());

    finalize(code, res.as<ireg>());
}
void Arm64::i32extend16_s(SHARED_PARAMS) {
    auto [p1, res] =
        allocate_registers<std::tuple<iwant::ireg>, iwant::ireg>(code);

    raw::sxth(code, false, p1.as<ireg>(), res.as<ireg>());

    finalize(code, res.as<ireg>());
}
void Arm64::i64extend8_s(SHARED_PARAMS) {
    auto [p1, res] =
        allocate_registers<std::tuple<iwant::ireg>, iwant::ireg>(code);

    raw::sxtb(code, true, p1.as<ireg>(), res.as<ireg>());

    finalize(code, res.as<ireg>());
}
void Arm64::i64extend16_s(SHARED_PARAMS) {
    auto [p1, res] =
        allocate_registers<std::tuple<iwant::ireg>, iwant::ireg>(code);

    raw::sxth(code, true, p1.as<ireg>(), res.as<ireg>());

    finalize(code, res.as<ireg>());
}
void Arm64::i64extend32_s(SHARED_PARAMS) {
    auto [p1, res] =
        allocate_registers<std::tuple<iwant::ireg>, iwant::ireg>(code);

    raw::sxtw(code, p1.as<ireg>(), res.as<ireg>());

    finalize(code, res.as<ireg>());
}
void Arm64::ref_null(SHARED_PARAMS) {
    auto [res] = allocate_registers<std::tuple<>, iwant::ireg>(code);
    raw::mov(code, true, ireg::xzr, res.as<ireg>());
    finalize(code, res.as<ireg>());
}
void Arm64::ref_is_null(SHARED_PARAMS) { i64eqz(code, stack); }
void Arm64::ref_func(SHARED_PARAMS, uint64_t misc_offset) {
    auto [res] = allocate_registers<std::tuple<>, iwant::ireg>(code);
    masm::ldr(code, intregs, true, misc_offset * sizeof(void *), miscreg,
              res.as<ireg>());
    finalize(code, res.as<ireg>());
}
void Arm64::ref_eq(SHARED_PARAMS) { i64eq(code, stack); }
void Arm64::i32_trunc_sat_f32_s(SHARED_PARAMS) {
    auto [p1, res] =
        allocate_registers<std::tuple<iwant::freg>, iwant::ireg>(code);
    raw::fcvtzs(code, false, ftype::single, p1.as<freg>(), res.as<ireg>());
    finalize(code, res.as<ireg>());
}
void Arm64::i32_trunc_sat_f32_u(SHARED_PARAMS) {
    auto [p1, res] =
        allocate_registers<std::tuple<iwant::freg>, iwant::ireg>(code);
    raw::fcvtzu(code, false, ftype::single, p1.as<freg>(), res.as<ireg>());
    finalize(code, res.as<ireg>());
}
void Arm64::i32_trunc_sat_f64_s(SHARED_PARAMS) {
    auto [p1, res] =
        allocate_registers<std::tuple<iwant::freg>, iwant::ireg>(code);
    raw::fcvtzs(code, false, ftype::double_, p1.as<freg>(), res.as<ireg>());
    finalize(code, res.as<ireg>());
}
void Arm64::i32_trunc_sat_f64_u(SHARED_PARAMS) {
    auto [p1, res] =
        allocate_registers<std::tuple<iwant::freg>, iwant::ireg>(code);
    raw::fcvtzu(code, false, ftype::double_, p1.as<freg>(), res.as<ireg>());
    finalize(code, res.as<ireg>());
}
void Arm64::i64_trunc_sat_f32_s(SHARED_PARAMS) {
    auto [p1, res] =
        allocate_registers<std::tuple<iwant::freg>, iwant::ireg>(code);
    raw::fcvtzs(code, true, ftype::single, p1.as<freg>(), res.as<ireg>());
    finalize(code, res.as<ireg>());
}
void Arm64::i64_trunc_sat_f32_u(SHARED_PARAMS) {
    auto [p1, res] =
        allocate_registers<std::tuple<iwant::freg>, iwant::ireg>(code);
    raw::fcvtzu(code, true, ftype::single, p1.as<freg>(), res.as<ireg>());
    finalize(code, res.as<ireg>());
}
void Arm64::i64_trunc_sat_f64_s(SHARED_PARAMS) {
    auto [p1, res] =
        allocate_registers<std::tuple<iwant::freg>, iwant::ireg>(code);
    raw::fcvtzs(code, true, ftype::double_, p1.as<freg>(), res.as<ireg>());
    finalize(code, res.as<ireg>());
}
void Arm64::i64_trunc_sat_f64_u(SHARED_PARAMS) {
    auto [p1, res] =
        allocate_registers<std::tuple<iwant::freg>, iwant::ireg>(code);
    raw::fcvtzu(code, true, ftype::double_, p1.as<freg>(), res.as<ireg>());
    finalize(code, res.as<ireg>());
}

template <runtime::Signature func, size_t NP, size_t NR>
void Arm64::runtime_call(std::byte *&code, std::array<valtype, NP> params,
                         std::array<valtype, NR> results,
                         std::optional<uint64_t> temp1,
                         std::optional<uint64_t> temp2) {
    allocate_registers<std::tuple<>>(code);

    clobber_flags(code);
    clobber_registers();

    auto vparams = valtype_vector(params), vresults = valtype_vector(results);
    stackify(code, vparams);

    masm::add(code, intregs, true, stack_size + vparams.bytesize(), stackreg,
              stackreg);
    if (temp1)
        masm::mov(code, *temp1, ireg::x3);
    if (temp2)
        masm::mov(code, *temp2, ireg::x4);
    masm::mov(code, reinterpret_cast<uint64_t>(func), ireg::x5);
    raw::blr(code, ireg::x5);

    masm::sub(code, intregs, true, stack_size + vresults.bytesize(), stackreg,
              stackreg);

    for ([[maybe_unused]] auto result : results)
        push(value::stack(stack_size));

    finalize(code);
}

void Arm64::memory_init(SHARED_PARAMS, uint64_t misc_offset) {
    runtime_call<runtime::memory_init>(
        code, std::to_array({valtype::i32, valtype::i32, valtype::i32}),
        std::array<valtype, 0>{}, misc_offset);
}
void Arm64::data_drop(SHARED_PARAMS, uint64_t misc_offset) {
    runtime_call<runtime::data_drop>(code, std::array<valtype, 0>{},
                                     std::array<valtype, 0>{}, misc_offset);
}
void Arm64::memory_copy(SHARED_PARAMS) {
    runtime_call<runtime::memory_copy>(
        code, std::to_array({valtype::i32, valtype::i32, valtype::i32}),
        std::array<valtype, 0>{});
}
void Arm64::memory_fill(SHARED_PARAMS) {
    runtime_call<runtime::memory_fill>(
        code, std::to_array({valtype::i32, valtype::i32, valtype::i32}),
        std::array<valtype, 0>{});
}
void Arm64::table_init(SHARED_PARAMS, uint64_t seg_offset,
                       uint64_t table_offset) {
    runtime_call<runtime::table_init>(
        code, std::to_array({valtype::i32, valtype::i32, valtype::i32}),
        std::array<valtype, 0>{}, seg_offset, table_offset);
}
void Arm64::elem_drop(SHARED_PARAMS, uint64_t misc_offset) {
    runtime_call<runtime::elem_drop>(code, std::array<valtype, 0>{},
                                     std::array<valtype, 0>{}, misc_offset);
}
void Arm64::table_copy(SHARED_PARAMS, uint64_t dst_offset,
                       uint64_t src_offset) {
    runtime_call<runtime::table_copy>(
        code, std::to_array({valtype::i32, valtype::i32, valtype::i32}),
        std::array<valtype, 0>{}, dst_offset, src_offset);
}
void Arm64::table_grow(SHARED_PARAMS, uint64_t misc_offset) {
    runtime_call<runtime::table_grow>(
        code, std::to_array({valtype::i32, valtype::externref}),
        std::to_array({valtype::i32}), misc_offset);
}
void Arm64::table_size(SHARED_PARAMS, uint64_t misc_offset) {
    auto [res] = allocate_registers<std::tuple<>, iwant::ireg>(code);

    masm::ldr(code, intregs, true, misc_offset * sizeof(void *), miscreg,
              res.as<ireg>());
    raw::ldr(code, true, res.as<ireg>(), res.as<ireg>());

    finalize(code, res.as<ireg>());
}
void Arm64::table_fill(SHARED_PARAMS, uint64_t misc_offset) {
    runtime_call<runtime::table_fill>(
        code, std::to_array({valtype::i32, valtype::externref, valtype::i32}),
        std::array<valtype, 0>{}, misc_offset);
}

} // namespace arm64
} // namespace mitey