#include "./baseline.hpp"
#include <cassert>
#include <cstring>
#include <limits>
#include <optional>

namespace mitey {
namespace arm64 {

namespace {

using inst = uint32_t;
static constexpr inst noop = 0xd503201f;

template <typename T> void put(std::byte *&code, const T &val) {
    std::memcpy(code, &val, sizeof(T));
    code += sizeof(T);
}

template <runtime::TrapKind kind> void trap(std::byte *&code) {
    // put address of runtime::trap in x1
    // put kind in x0
    // br x1
}

void orr(std::byte *&code, bool sf, shifttype shift, ireg rm, uint8_t shift_imm,
         ireg rn, ireg rd) {
    put(code, 0b00101010000000000000000000000000 |
                  (static_cast<uint32_t>(sf) << 31) |
                  (static_cast<uint32_t>(shift) << 22) |
                  (static_cast<uint32_t>(rm) << 16) |
                  (static_cast<uint32_t>(shift_imm) << 10) |
                  (static_cast<uint32_t>(rn) << 5) |
                  (static_cast<uint32_t>(rd) << 0));
}

void add(std::byte *&code, bool sf, uint16_t imm12, ireg rn, ireg rd) {
    put(code, 0b00010001000000000000000000000000 |
                  (static_cast<uint32_t>(sf) << 31) |
                  (static_cast<uint32_t>(imm12) << 10) |
                  (static_cast<uint32_t>(rn) << 5) |
                  (static_cast<uint32_t>(rd) << 0));
}

void add(std::byte *&code, bool sf, ireg rm, ireg rn, ireg rd) {
    put(code, 0b00001011000000000000000000000000 |
                  (static_cast<uint32_t>(sf) << 31) |
                  (static_cast<uint32_t>(rm) << 16) |
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
    csinc(code, sf, ireg::xzr, c, ireg::xzr, rd);
}

void mov(std::byte *&code, ireg src, ireg dst) {
    orr(code, true, shifttype::lsl, src, 0, ireg::xzr, dst);
}

void mov(std::byte *&code, ireg src, freg dst) {
    put(code, 0b10011110011001110000000000000000 |
                  (static_cast<uint32_t>(src) << 5) |
                  (static_cast<uint32_t>(dst) << 0));
}

void mov(std::byte *&code, freg src, ireg dst) {
    put(code, 0b10011110011001100000000000000000 |
                  (static_cast<uint32_t>(src) << 5) |
                  (static_cast<uint32_t>(dst) << 0));
}

void mov(std::byte *&code, freg src, freg dst) {
    put(code, 0b10011110011001110000000000000000 |
                  (static_cast<uint32_t>(src) << 5) |
                  (static_cast<uint32_t>(dst) << 0));
}

void mov(std::byte *&code, bool sf, bool notneg, bool keep, uint8_t hw,
         uint16_t imm, ireg rd) {
    put(code, 0b00010010100000000000000000000000 |
                  (static_cast<uint32_t>(sf) << 31) |
                  (static_cast<uint32_t>(notneg) << 30) |
                  (static_cast<uint32_t>(keep) << 29) |
                  (static_cast<uint32_t>(hw) << 21) |
                  (static_cast<uint32_t>(imm) << 5) |
                  (static_cast<uint8_t>(rd) << 0));
}

void mov(std::byte *&code, uint64_t imm, ireg dst) {
    // todo: should this have a fast path for 0?
    // i think that should be handled elsewhere
    bool keep = false;
    for (size_t i = 0; i < sizeof(uint32_t) && imm; i++) {
        auto literal = imm & 0xffff;
        imm >>= 16;
        mov(code, true, true, keep, literal, i, dst);
        keep = true;
    }
}

void str_offset(std::byte *&code, uint32_t offset, ireg rn, ireg rt) {
    offset /= 8;
    put(code, 0b11111001000000000000000000000000 |
                  (static_cast<uint32_t>(offset) << 10) |
                  (static_cast<uint32_t>(rn) << 5) |
                  (static_cast<uint32_t>(rt) << 0));
}

void str_offset(std::byte *&code, uint32_t offset, ireg rn, freg rt) {
    offset /= 8;
    put(code, 0b11111101000000000000000000000000 |
                  (static_cast<uint32_t>(offset) << 10) |
                  (static_cast<uint32_t>(rn) << 5) |
                  (static_cast<uint32_t>(rt) << 0));
}

void ldr_offset(std::byte *&code, uint32_t offset, ireg rn, ireg rt) {
    offset /= 8;
    put(code, 0b11111001010000000000000000000000 |
                  (static_cast<uint32_t>(offset) << 10) |
                  (static_cast<uint32_t>(rn) << 5) |
                  (static_cast<uint32_t>(rt) << 0));
}

void ldr_offset(std::byte *&code, uint32_t offset, ireg rn, freg rt) {
    offset /= 8;
    put(code, 0b11111101010000000000000000000000 |
                  (static_cast<uint32_t>(offset) << 10) |
                  (static_cast<uint32_t>(rn) << 5) |
                  (static_cast<uint32_t>(rt) << 0));
}

struct LogicalImm {
    uint32_t prefix : 9;
    uint32_t N : 1;
    uint32_t immr : 6;
    uint32_t imms : 6;
    uint32_t postfix : 10;
};
static_assert(sizeof(LogicalImm) == sizeof(uint32_t));

// based on
// https://dougallj.wordpress.com/2021/10/30/bit-twiddling-optimising-aarch64-logical-immediate-encoding-and-decoding/
std::optional<LogicalImm> tryLogicalImm(uint64_t val) {
    if (val == 0 || ~val == 0)
        return std::nullopt;

    uint32_t rotation = std::countr_zero(val & (val + 1));
    uint64_t normalized = std::rotr(val, rotation & 63);

    uint32_t zeroes = std::countl_zero(normalized);
    uint32_t ones = std::countr_one(normalized);
    uint32_t size = zeroes + ones;

    if (std::rotr(val, size & 63) != val)
        return std::nullopt;

    return LogicalImm{.N = (size >> 6),
                      .immr = -rotation & (size - 1),
                      .imms = (-(size << 1) | (ones - 1)) & 0x3f};
}

std::optional<LogicalImm> tryLogicalImm(uint32_t val) {
    uint64_t val64 = ((uint64_t)val << 32) | val;
    return tryLogicalImm(val64);
}

bool is_volatile(ireg reg) { return reg <= icaller_saved.back(); }
bool is_volatile(freg reg) { return reg <= fcaller_saved.back(); }

}; // namespace

void Arm64::clobber_flags(std::byte *&code) {
    if (!flag.val)
        return;

    // step 1. claim a register, spilling if necessary
    auto [reg, metadata] = intregs.steal(code);
    // step 2. spill into claimed register
    cset(code, true, flag.val->as<cond>(), reg);
    // step 3. set register metadata (for spilling)
    *metadata = decltype(intregs)::metadata(code, flag.stack_offset);

    *flag.val = value::reg(reg);
    flag.val = nullptr;
}

void Arm64::push(value v) {
    *values++ = v;
    // consts don't occupy stack space
    if (!v.is<value::location::imm>()) {
        stack_size += sizeof(runtime::WasmValue);
    }
}

template <size_t nparams>
std::array<value, nparams + 1>
Arm64::allocate_registers(std::byte *&code,
                          std::array<poption::type, nparams> params,
                          poption::type result) {

    std::array<value, nparams + 1> ret;
    bool return_set = false;

    values -= nparams;
    for (int i = 0; i < nparams; i++) {
        auto v = values[i];

        switch (v.where()) {
        case value::location::reg: {
            stack_size -= sizeof(runtime::WasmValue);

            // already in a register - good to go
            ret[i] = v;
            break;
        }
        case value::location::stack: {
            stack_size -= sizeof(runtime::WasmValue);

            auto offset = v.as<uint32_t>();
            if (params[i] == poption::freg) {
                auto [reg, _] = floatregs.steal(code);
                ldr_offset(code, offset, stackreg, reg);
                ret[i] = value::reg(reg);
            } else {
                auto [reg, _] = intregs.steal(code);
                ldr_offset(code, offset, stackreg, reg);
                ret[i] = value::reg(reg);
            }
            break;
        }
        case value::location::imm: {
            assert(params[i] != poption::freg);

            auto imm = v.as<uint32_t>();
            if (params[i].fits(imm)) {
                ret[i] = v;
            } else if (auto mask = tryLogicalImm(imm);
                       params[i] == poption::bitmask && mask) {
                ret[i] = value::imm(std::bit_cast<uint32_t>(*mask));
            } else {
                auto [reg, _] = intregs.steal(code);
                mov(code, imm, reg);
                ret[i] = value::reg(reg);
            }
            break;
        }
        case value::location::flag: {
            stack_size -= sizeof(runtime::WasmValue);

            assert(params[i] != poption::freg);

            if (params[i] == poption::flags) {
                ret[i] = v;
            } else {
                auto [reg, _] = intregs.steal(code);
                cset(code, true, v.as<cond>(), reg);
                ret[i] = value::reg(reg);
            }
            break;
        }
        }

        if (params[i] == result &&
            ((result == poption::ireg && is_volatile(v.as<ireg>())) ||
             (result == poption::freg && is_volatile(v.as<freg>())))) {
            ret.back() = ret[i];
            return_set = true;
        }
    }

    for (int i = 0; i < nparams; i++) {
        if (params[i] == poption::freg) {
            floatregs.surrender(ret[i].template as<freg>());
        } else {
            intregs.surrender(ret[i].template as<ireg>());
        }
    }

    if (result != poption::none && !return_set) {
        if (result == poption::freg) {
            auto [reg, _] = floatregs.steal(code);
            ret.back() = value::reg(reg);
        } else {
            auto [reg, _] = intregs.steal(code);
            ret.back() = value::reg(reg);
        }
    }

    return ret;
}

void Arm64::finalize(std::byte *&code, ireg result) {
    intregs.claim(result, decltype(intregs)::metadata(code, stack_size));
    // buffer area for spilling
    code += sizeof(inst);
    stack_size += sizeof(runtime::WasmValue);

    *values++ = value::reg(result);
}

void Arm64::finalize(std::byte *&code, freg result) {
    floatregs.claim(result, decltype(floatregs)::metadata(code, stack_size));
    // buffer area for spilling
    code += sizeof(inst);
    stack_size += sizeof(runtime::WasmValue);

    *values++ = value::reg(result);
}

void Arm64::start_function(SHARED_PARAMS, FunctionShell &fn) {
    // stp     x29, x30, [sp, #-0x10]!
    // mov     x29, sp
    put(code, std::array<uint32_t, 2>{0xa9bf7bfd, 0x910003fd});

    locals = std::span(new value[fn.locals.size()], fn.locals.size());

    auto ireg_alloc = icallee_saved.begin();
    auto freg_alloc = fcallee_saved.begin();

    for (auto i = 0; i < fn.locals.size(); i++) {
        auto local = fn.locals[i];
        auto offset = i * sizeof(runtime::WasmValue);
        auto is_param = i < fn.type.params.size();

        if ((local == valtype::i32 || local == valtype::i64 ||
             local == valtype::funcref || local == valtype::externref) &&
            ireg_alloc != icallee_saved.end()) {
            auto reg = *ireg_alloc++;
            // save current value
            if (is_param) {
                mov(code, reg, ireg::x3);
                ldr_offset(code, offset, stackreg, reg);
                str_offset(code, offset, stackreg, ireg::x3);
            } else {
                str_offset(code, offset, stackreg, reg);
                mov(code, ireg::xzr, reg);
            }

            locals[i] = value::reg(reg);
        } else if ((local == valtype::f32 || local == valtype::f64) &&
                   freg_alloc != fcallee_saved.end()) {
            auto reg = *freg_alloc++;
            // save current value
            if (is_param) {
                mov(code, reg, freg::d0);
                ldr_offset(code, offset, stackreg, reg);
                str_offset(code, offset, stackreg, freg::d0);
            } else {
                str_offset(code, offset, stackreg, reg);
                mov(code, ireg::xzr, reg);
            }

            locals[i] = value::reg(reg);
        } else {
            str_offset(code, offset, stackreg, ireg::xzr);
            locals[i] = value::stack(offset);
        }
    }
}
void Arm64::exit_function(SHARED_PARAMS, FunctionShell &fn) {
    // restore saved values
    for (auto i = 0; i < fn.type.params.size(); i++) {
        if (!locals[i].is<value::location::reg>())
            continue;

        auto local = fn.locals[i];
        auto offset = i * sizeof(runtime::WasmValue);

        if (local == valtype::i32 || local == valtype::i64 ||
            local == valtype::funcref || local == valtype::externref) {
            ldr_offset(code, offset, stackreg, locals[i].as<ireg>());
        } else if (local == valtype::f32 || local == valtype::f64) {
            ldr_offset(code, offset, stackreg, locals[i].as<freg>());
        }
    }

    // ldp     x29, x30, [sp], #0x10
    // ret
    put(code, std::array<uint32_t, 2>{0xa8c17bfd, 0xd65f03c0});

    delete[] locals.data();
}

void Arm64::unreachable(SHARED_PARAMS) {
    trap<runtime::TrapKind::unreachable>(code);
}
void Arm64::nop(SHARED_PARAMS) { put(code, noop); }
// void Arm64::block(SHARED_PARAMS, WasmSignature &sig);
// void Arm64::loop(SHARED_PARAMS, WasmSignature &sig);
// std::byte *Arm64::if_(SHARED_PARAMS, WasmSignature &sig);
// std::byte *Arm64::else_(SHARED_PARAMS, WasmSignature &sig,
//                         std::byte *if_location);
// void Arm64::end(SHARED_PARAMS, ControlFlow &flow);
// void Arm64::br(SHARED_PARAMS, std::span<ControlFlow> control_stack,
//                uint32_t depth);
// void Arm64::br_if(SHARED_PARAMS, std::span<ControlFlow> control_stack,
//                   uint32_t depth);
// void Arm64::br_table(SHARED_PARAMS, std::span<ControlFlow> control_stack,
//                      std::span<uint32_t> targets);
// void Arm64::return_(SHARED_PARAMS, std::span<ControlFlow> control_stack);
// void Arm64::call(SHARED_PARAMS, FunctionShell &fn, uint32_t func_offset);
// void Arm64::call_indirect(SHARED_PARAMS, uint32_t table_offset,
//                           WasmSignature &type);
// void Arm64::drop(SHARED_PARAMS);
// void Arm64::select(SHARED_PARAMS);
// void Arm64::select_t(SHARED_PARAMS);
void Arm64::localget(SHARED_PARAMS, FunctionShell &fn, uint32_t local_idx) {
    push(locals[local_idx]);
}
// void Arm64::localset(SHARED_PARAMS, FunctionShell &fn, uint32_t local_idx);
// void Arm64::localtee(SHARED_PARAMS, FunctionShell &fn, uint32_t local_idx);
// void Arm64::tableget(SHARED_PARAMS, uint64_t misc_offset);
// void Arm64::tableset(SHARED_PARAMS, uint64_t misc_offset);
// void Arm64::globalget(SHARED_PARAMS, uint64_t misc_offset);
// void Arm64::globalset(SHARED_PARAMS, uint64_t misc_offset);
// void Arm64::memorysize(SHARED_PARAMS);
// void Arm64::memorygrow(SHARED_PARAMS);
void Arm64::i32const(SHARED_PARAMS, uint32_t cons) { push(value::imm(cons)); }
void Arm64::i64const(SHARED_PARAMS, uint64_t cons) {
    if (cons <= std::numeric_limits<uint32_t>::max()) {
        // mov(code, cons, ireg::x0);
    } else {
        push(value::imm(cons));
    }
}
void Arm64::f32const(SHARED_PARAMS, float cons) {
    // mov(code, ireg::x0, std::bit_cast<uint32_t>(cons));
    // mov(code, freg::d0, ireg::x0);
}
void Arm64::f64const(SHARED_PARAMS, double cons) {
    // mov(code, ireg::x0, std::bit_cast<uint64_t>(cons));
    // mov(code, freg::d0, ireg::x0);
}
// void Arm64::i32load(SHARED_PARAMS, uint64_t offset, uint64_t align);
// void Arm64::i64load(SHARED_PARAMS, uint64_t offset, uint64_t align);
// void Arm64::f32load(SHARED_PARAMS, uint64_t offset, uint64_t align);
// void Arm64::f64load(SHARED_PARAMS, uint64_t offset, uint64_t align);
// void Arm64::i32load8_s(SHARED_PARAMS, uint64_t offset, uint64_t align);
// void Arm64::i32load8_u(SHARED_PARAMS, uint64_t offset, uint64_t align);
// void Arm64::i32load16_s(SHARED_PARAMS, uint64_t offset, uint64_t align);
// void Arm64::i32load16_u(SHARED_PARAMS, uint64_t offset, uint64_t align);
// void Arm64::i64load8_s(SHARED_PARAMS, uint64_t offset, uint64_t align);
// void Arm64::i64load8_u(SHARED_PARAMS, uint64_t offset, uint64_t align);
// void Arm64::i64load16_s(SHARED_PARAMS, uint64_t offset, uint64_t align);
// void Arm64::i64load16_u(SHARED_PARAMS, uint64_t offset, uint64_t align);
// void Arm64::i64load32_s(SHARED_PARAMS, uint64_t offset, uint64_t align);
// void Arm64::i64load32_u(SHARED_PARAMS, uint64_t offset, uint64_t align);
// void Arm64::i32store(SHARED_PARAMS, uint64_t offset, uint64_t align);
// void Arm64::i64store(SHARED_PARAMS, uint64_t offset, uint64_t align);
// void Arm64::f32store(SHARED_PARAMS, uint64_t offset, uint64_t align);
// void Arm64::f64store(SHARED_PARAMS, uint64_t offset, uint64_t align);
// void Arm64::i32store8(SHARED_PARAMS, uint64_t offset, uint64_t align);
// void Arm64::i32store16(SHARED_PARAMS, uint64_t offset, uint64_t align);
// void Arm64::i64store8(SHARED_PARAMS, uint64_t offset, uint64_t align);
// void Arm64::i64store16(SHARED_PARAMS, uint64_t offset, uint64_t align);
// void Arm64::i64store32(SHARED_PARAMS, uint64_t offset, uint64_t align);
// void Arm64::i32eqz(SHARED_PARAMS);
// void Arm64::i64eqz(SHARED_PARAMS);
// void Arm64::i32eq(SHARED_PARAMS);
// void Arm64::i64eq(SHARED_PARAMS);
// void Arm64::i32ne(SHARED_PARAMS);
// void Arm64::i64ne(SHARED_PARAMS);
// void Arm64::i32lt_s(SHARED_PARAMS);
// void Arm64::i64lt_s(SHARED_PARAMS);
// void Arm64::i32lt_u(SHARED_PARAMS);
// void Arm64::i64lt_u(SHARED_PARAMS);
// void Arm64::i32gt_s(SHARED_PARAMS);
// void Arm64::i64gt_s(SHARED_PARAMS);
// void Arm64::i32gt_u(SHARED_PARAMS);
// void Arm64::i64gt_u(SHARED_PARAMS);
// void Arm64::i32le_s(SHARED_PARAMS);
// void Arm64::i64le_s(SHARED_PARAMS);
// void Arm64::i32le_u(SHARED_PARAMS);
// void Arm64::i64le_u(SHARED_PARAMS);
// void Arm64::i32ge_s(SHARED_PARAMS);
// void Arm64::i64ge_s(SHARED_PARAMS);
// void Arm64::i32ge_u(SHARED_PARAMS);
// void Arm64::i64ge_u(SHARED_PARAMS);
// void Arm64::f32eq(SHARED_PARAMS);
// void Arm64::f64eq(SHARED_PARAMS);
// void Arm64::f32ne(SHARED_PARAMS);
// void Arm64::f64ne(SHARED_PARAMS);
// void Arm64::f32lt(SHARED_PARAMS);
// void Arm64::f64lt(SHARED_PARAMS);
// void Arm64::f32gt(SHARED_PARAMS);
// void Arm64::f64gt(SHARED_PARAMS);
// void Arm64::f32le(SHARED_PARAMS);
// void Arm64::f64le(SHARED_PARAMS);
// void Arm64::f32ge(SHARED_PARAMS);
// void Arm64::f64ge(SHARED_PARAMS);
// void Arm64::i32clz(SHARED_PARAMS);
// void Arm64::i64clz(SHARED_PARAMS);
// void Arm64::i32ctz(SHARED_PARAMS);
// void Arm64::i64ctz(SHARED_PARAMS);
// void Arm64::i32popcnt(SHARED_PARAMS);
// void Arm64::i64popcnt(SHARED_PARAMS);
void Arm64::i32add(SHARED_PARAMS) {
    auto [p1, p2, res] = allocate_registers(
        code, std::array{poption::ireg, poption::literal<1 << 12>},
        poption::ireg);

    if (p2.is<value::location::imm>()) {
        add(code, false, p2.as<uint32_t>(), p1.as<ireg>(), res.as<ireg>());
    } else {
        add(code, false, p2.as<ireg>(), p1.as<ireg>(), res.as<ireg>());
    }

    finalize(code, res.as<ireg>());
}
// void Arm64::i64add(SHARED_PARAMS);
// void Arm64::i32sub(SHARED_PARAMS);
// void Arm64::i64sub(SHARED_PARAMS);
// void Arm64::i32mul(SHARED_PARAMS);
// void Arm64::i64mul(SHARED_PARAMS);
// void Arm64::i32div_s(SHARED_PARAMS);
// void Arm64::i64div_s(SHARED_PARAMS);
// void Arm64::i32div_u(SHARED_PARAMS);
// void Arm64::i64div_u(SHARED_PARAMS);
// void Arm64::i32rem_s(SHARED_PARAMS);
// void Arm64::i64rem_s(SHARED_PARAMS);
// void Arm64::i32rem_u(SHARED_PARAMS);
// void Arm64::i64rem_u(SHARED_PARAMS);
// void Arm64::i32and(SHARED_PARAMS);
// void Arm64::i64and(SHARED_PARAMS);
// void Arm64::i32or(SHARED_PARAMS);
// void Arm64::i64or(SHARED_PARAMS);
// void Arm64::i32xor(SHARED_PARAMS);
// void Arm64::i64xor(SHARED_PARAMS);
// void Arm64::i32shl(SHARED_PARAMS);
// void Arm64::i64shl(SHARED_PARAMS);
// void Arm64::i32shr_s(SHARED_PARAMS);
// void Arm64::i64shr_s(SHARED_PARAMS);
// void Arm64::i32shr_u(SHARED_PARAMS);
// void Arm64::i64shr_u(SHARED_PARAMS);
// void Arm64::i32rotl(SHARED_PARAMS);
// void Arm64::i64rotl(SHARED_PARAMS);
// void Arm64::i32rotr(SHARED_PARAMS);
// void Arm64::i64rotr(SHARED_PARAMS);
// void Arm64::f32abs(SHARED_PARAMS);
// void Arm64::f64abs(SHARED_PARAMS);
// void Arm64::f32neg(SHARED_PARAMS);
// void Arm64::f64neg(SHARED_PARAMS);
// void Arm64::f32ceil(SHARED_PARAMS);
// void Arm64::f64ceil(SHARED_PARAMS);
// void Arm64::f32floor(SHARED_PARAMS);
// void Arm64::f64floor(SHARED_PARAMS);
// void Arm64::f32trunc(SHARED_PARAMS);
// void Arm64::f64trunc(SHARED_PARAMS);
// void Arm64::f32nearest(SHARED_PARAMS);
// void Arm64::f64nearest(SHARED_PARAMS);
// void Arm64::f32sqrt(SHARED_PARAMS);
// void Arm64::f64sqrt(SHARED_PARAMS);
// void Arm64::f32add(SHARED_PARAMS);
// void Arm64::f64add(SHARED_PARAMS);
// void Arm64::f32sub(SHARED_PARAMS);
// void Arm64::f64sub(SHARED_PARAMS);
// void Arm64::f32mul(SHARED_PARAMS);
// void Arm64::f64mul(SHARED_PARAMS);
// void Arm64::f32div(SHARED_PARAMS);
// void Arm64::f64div(SHARED_PARAMS);
// void Arm64::f32min(SHARED_PARAMS);
// void Arm64::f64min(SHARED_PARAMS);
// void Arm64::f32max(SHARED_PARAMS);
// void Arm64::f64max(SHARED_PARAMS);
// void Arm64::f32copysign(SHARED_PARAMS);
// void Arm64::f64copysign(SHARED_PARAMS);
// void Arm64::i32wrap_i64(SHARED_PARAMS);
// void Arm64::i64extend_i32_s(SHARED_PARAMS);
// void Arm64::i64extend_i32_u(SHARED_PARAMS);
// void Arm64::i32trunc_f32_s(SHARED_PARAMS);
// void Arm64::i64trunc_f32_s(SHARED_PARAMS);
// void Arm64::i32trunc_f32_u(SHARED_PARAMS);
// void Arm64::i64trunc_f32_u(SHARED_PARAMS);
// void Arm64::i32trunc_f64_s(SHARED_PARAMS);
// void Arm64::i64trunc_f64_s(SHARED_PARAMS);
// void Arm64::i32trunc_f64_u(SHARED_PARAMS);
// void Arm64::i64trunc_f64_u(SHARED_PARAMS);
// void Arm64::f32convert_i32_s(SHARED_PARAMS);
// void Arm64::f64convert_i32_s(SHARED_PARAMS);
// void Arm64::f32convert_i32_u(SHARED_PARAMS);
// void Arm64::f64convert_i32_u(SHARED_PARAMS);
// void Arm64::f32convert_i64_s(SHARED_PARAMS);
// void Arm64::f64convert_i64_s(SHARED_PARAMS);
// void Arm64::f32convert_i64_u(SHARED_PARAMS);
// void Arm64::f64convert_i64_u(SHARED_PARAMS);
// void Arm64::f32demote_f64(SHARED_PARAMS);
// void Arm64::f64promote_f32(SHARED_PARAMS);
// void Arm64::i32reinterpret_f32(SHARED_PARAMS);
// void Arm64::f32reinterpret_i32(SHARED_PARAMS);
// void Arm64::i64reinterpret_f64(SHARED_PARAMS);
// void Arm64::f64reinterpret_i64(SHARED_PARAMS);
// void Arm64::i32extend8_s(SHARED_PARAMS);
// void Arm64::i32extend16_s(SHARED_PARAMS);
// void Arm64::i64extend8_s(SHARED_PARAMS);
// void Arm64::i64extend16_s(SHARED_PARAMS);
// void Arm64::i64extend32_s(SHARED_PARAMS);
// void Arm64::ref_null(SHARED_PARAMS);
// void Arm64::ref_is_null(SHARED_PARAMS);
// void Arm64::ref_func(SHARED_PARAMS, uint64_t misc_offset);
// void Arm64::ref_eq(SHARED_PARAMS);
// void Arm64::i32_trunc_sat_f32_s(SHARED_PARAMS);
// void Arm64::i32_trunc_sat_f32_u(SHARED_PARAMS);
// void Arm64::i32_trunc_sat_f64_s(SHARED_PARAMS);
// void Arm64::i32_trunc_sat_f64_u(SHARED_PARAMS);
// void Arm64::i64_trunc_sat_f32_s(SHARED_PARAMS);
// void Arm64::i64_trunc_sat_f32_u(SHARED_PARAMS);
// void Arm64::i64_trunc_sat_f64_s(SHARED_PARAMS);
// void Arm64::i64_trunc_sat_f64_u(SHARED_PARAMS);
// void Arm64::memory_init(SHARED_PARAMS, uint64_t misc_offset);
// void Arm64::data_drop(SHARED_PARAMS, uint64_t misc_offset);
// void Arm64::memory_copy(SHARED_PARAMS);
// void Arm64::memory_fill(SHARED_PARAMS);
// void Arm64::table_init(SHARED_PARAMS, uint64_t seg_offset,
//                        uint64_t table_offset);
// void Arm64::elem_drop(SHARED_PARAMS, uint64_t misc_offset);
// void Arm64::table_copy(SHARED_PARAMS, uint64_t dst_offset, uint64_t
// src_offset); void Arm64::table_grow(SHARED_PARAMS, uint64_t misc_offset);
// void Arm64::table_size(SHARED_PARAMS, uint64_t misc_offset);
// void Arm64::table_fill(SHARED_PARAMS, uint64_t misc_offset);

} // namespace arm64
} // namespace mitey