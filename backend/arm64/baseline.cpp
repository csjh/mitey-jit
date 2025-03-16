#include "./baseline.hpp"
#include "../../type-templates.hpp"
#include <cassert>
#include <cstring>
#include <limits>
#include <optional>
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

void addsub(std::byte *&code, bool sf, bool sub, bool setflags, bool shift,
            uint16_t imm12, ireg rn, ireg rd) {
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
    put(code, 0b00001011000000000000000000000000 |
                  (static_cast<uint32_t>(sf) << 31) |
                  (static_cast<uint32_t>(sub) << 30) |
                  (static_cast<uint32_t>(setflags) << 29) |
                  (static_cast<uint32_t>(rm) << 16) |
                  (static_cast<uint32_t>(shift_n) << 10) |
                  (static_cast<uint32_t>(rn) << 5) |
                  (static_cast<uint32_t>(rd) << 0));
}

void add(std::byte *&code, bool sf, ireg rd, ireg rn, uint16_t imm12,
         bool shift = false) {
    addsub(code, sf, false, false, shift, imm12, rn, rd);
}

void add(std::byte *&code, bool sf, ireg rd, ireg rm, ireg rn,
         shifttype shift = shifttype::lsl, uint8_t shift_n = 0) {
    addsub(code, sf, false, false, shift, rm, shift_n, rn, rd);
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

void b(std::byte *&code, int32_t _imm26) {
    uint32_t imm26 = _imm26 / sizeof(uint32_t);
    imm26 &= 0x3ffffff;

    put(code, 0b00010100000000000000000000000000 |
                  (static_cast<uint32_t>(imm26) << 0));
}

void bcond(std::byte *&code, int32_t _imm19, cond c) {
    uint32_t imm19 = _imm19 /= sizeof(uint32_t);
    imm19 &= 0x7ffff;

    put(code, 0b01010100000000000000000000000000 |
                  (static_cast<uint32_t>(imm19) << 5) |
                  (static_cast<uint32_t>(c) << 0));
}

void cbnz(std::byte *&code, bool sf, int32_t _imm19, ireg rt) {
    uint32_t imm19 = _imm19 /= sizeof(uint32_t);
    imm19 &= 0x7ffff;

    put(code, 0b00110101000000000000000000000000 |
                  (static_cast<uint32_t>(sf) << 31) |
                  (static_cast<uint32_t>(imm19) << 5) |
                  (static_cast<uint32_t>(rt) << 0));
}

void cbz(std::byte *&code, bool sf, int32_t _imm19, ireg rt) {
    uint32_t imm19 = _imm19 /= sizeof(uint32_t);
    imm19 &= 0x7ffff;

    put(code, 0b00110100000000000000000000000000 |
                  (static_cast<uint32_t>(sf) << 31) |
                  (static_cast<uint32_t>(imm19) << 5) |
                  (static_cast<uint32_t>(rt) << 0));
}

void fcmp(std::byte *&code, bool is_double, freg rn, freg rm) {
    put(code, 0b00011110001000000010000000010000 |
                  (static_cast<uint32_t>(is_double) << 22) |
                  (static_cast<uint32_t>(rm) << 16) |
                  (static_cast<uint32_t>(rn) << 5));
}

void clz(std::byte *&code, bool sf, ireg rd, ireg rn) {
    put(code, 0b01011010110000000001000000000000 |
                  (static_cast<uint32_t>(sf) << 31) |
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
    if (imm == 0) {
        mov(code, ireg::xzr, dst);
        return;
    }

    bool keep = false;
    for (size_t i = 0; i < sizeof(uint32_t) && imm; i++) {
        auto literal = imm & 0xffff;
        imm >>= 16;
        mov(code, true, true, keep, i, literal, dst);
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

template <typename RegType, size_t First, size_t Last>
void Arm64::reg_manager<RegType, First, Last>::spill(RegType reg) {
    auto [addr, offset] = data[to_index(reg)];
    if (addr)
        str_offset(addr, offset, stackreg, reg);
}

template <typename RegType, size_t First, size_t Last>
uint8_t Arm64::reg_manager<RegType, First, Last>::to_index(RegType reg) {
    return static_cast<uint8_t>(reg) - First;
}

template <typename RegType, size_t First, size_t Last>
RegType Arm64::reg_manager<RegType, First, Last>::from_index(uint8_t idx) {
    return static_cast<RegType>(idx + First);
}

template <typename RegType, size_t First, size_t Last>
void Arm64::reg_manager<RegType, First, Last>::begin() {
    regs.begin();
}

template <typename RegType, size_t First, size_t Last>
RegType Arm64::reg_manager<RegType, First, Last>::result(std::byte *&code) {
    auto idx = regs.result();
    spill(from_index(idx));
    return from_index(idx);
}

template <typename RegType, size_t First, size_t Last>
void Arm64::reg_manager<RegType, First, Last>::claim(RegType reg, metadata md) {
    auto idx = to_index(reg);
    data[idx] = md;
}

template <typename RegType, size_t First, size_t Last>
RegType Arm64::reg_manager<RegType, First, Last>::temporary(std::byte *&code) {
    auto idx = regs.temporary();
    spill(from_index(idx));
    data[idx] = metadata(nullptr, 0);
    return from_index(idx);
}

template <typename RegType, size_t First, size_t Last>
void Arm64::reg_manager<RegType, First, Last>::surrender(RegType reg) {
    auto idx = to_index(reg);
    regs.surrender(idx);
    data[idx] = metadata(nullptr, 0);
}

template <typename RegType, size_t First, size_t Last>
void Arm64::reg_manager<RegType, First, Last>::commit() {
    regs.commit();
}

template <typename RegType, size_t First, size_t Last>
void Arm64::reg_manager<RegType, First, Last>::clobber_all(std::byte *&code) {
    for (int i = 0; i < Last - First; i++) {
        spill(from_index(i));
    }
}

template <typename RegType, size_t First, size_t Last>
bool Arm64::reg_manager<RegType, First, Last>::check_spill(RegType reg,
                                                           std::byte *code) {
    return data[to_index(reg)].spilladdr == code;
}

void Arm64::clobber_flags(std::byte *&code) {
    if (!flag.val)
        return;

    // step 1. claim a register, spilling if necessary
    auto reg = intregs.result(code);
    // step 2. spill into claimed register
    cset(code, true, flag.val->as<cond>(), reg);
    // step 3. set register metadata (for spilling)
    intregs.claim(reg, decltype(intregs)::metadata(code, flag.stack_offset));
    put(code, noop);

    *flag.val = value::reg(reg);
    flag.val = nullptr;
}

void Arm64::clobber_registers(std::byte *&code) {
    intregs.clobber_all(code);
    floatregs.clobber_all(code);
}

void Arm64::push(value v) {
    *values++ = v;
    if (v.is<value::location::flags>())
        flag = flags(stack_size, values - 1);
    stack_size += sizeof(runtime::WasmValue);
}

template <typename To> value Arm64::adapt_value(std::byte *&code, value v) {
    using RegType =
        std::conditional_t<std::is_same_v<To, iwant::freg>, freg, ireg>;

    switch (v.where()) {
    case value::location::reg: {
        // why did I have the commented out part?
        // if (is_volatile(v.as<RegType>())) {
        //     RegType reg;
        //     if constexpr (std::is_same_v<To, iwant::freg>)
        //         reg = floatregs.temporary(code);
        //     else
        //         reg = intregs.temporary(code);
        //     mov(code, v.as<RegType>(), reg);
        //     return value::reg(reg);
        // } else {
        return v;
        // }
    }
    case value::location::stack: {
        auto offset = v.as<uint32_t>();
        RegType reg;
        if constexpr (std::is_same_v<To, iwant::freg>)
            reg = floatregs.temporary(code);
        else
            reg = intregs.temporary(code);
        ldr_offset(code, offset, stackreg, reg);
        return value::reg(reg);
    }
    case value::location::imm: {
        auto better_not = !std::is_same_v<To, iwant::freg>;
        assert(better_not);

        auto imm = v.as<uint32_t>();

        if constexpr (is_value_specialization_of<iwant::literal, To>)
            if (imm < To::threshold)
                return v;
        if constexpr (std::is_same_v<To, iwant::bitmask>)
            if (auto mask = tryLogicalImm(imm); mask)
                return value::imm(std::bit_cast<uint32_t>(*mask));

        auto reg = intregs.temporary(code);
        mov(code, imm, reg);
        return value::reg(reg);
    }
    case value::location::flags: {
        auto better_not = !std::is_same_v<To, iwant::freg>;
        assert(better_not);

        flag = flags();

        if constexpr (std::is_same_v<To, iwant::flags>) {
            return v;
        } else {
            auto reg = intregs.temporary(code);
            cset(code, true, v.as<cond>(), reg);
            return value::reg(reg);
        }
    }
    }

    assert(false);
}

ireg Arm64::adapt_value_into(std::byte *&code, value v,
                             std::optional<ireg> &hint) {
    if (v.is<value::location::reg>())
        return v.as<ireg>();

    if (!hint)
        hint = intregs.temporary(code);
    auto reg = *hint;

    switch (v.where()) {
    case value::location::reg: {
        __builtin_unreachable();
    }
    case value::location::stack: {
        auto offset = v.as<uint32_t>();
        ldr_offset(code, offset, stackreg, reg);
        return reg;
    }
    case value::location::imm: {
        auto imm = v.as<uint32_t>();
        mov(code, imm, reg);
        return reg;
    }
    case value::location::flags: {
        cset(code, true, v.as<cond>(), reg);
        return reg;
    }
    }

    assert(false);
}

freg Arm64::adapt_value_into(std::byte *&code, value v,
                             std::optional<freg> &hint) {
    if (v.is<value::location::reg>())
        return v.as<freg>();

    if (!hint)
        hint = floatregs.temporary(code);
    auto reg = *hint;

    switch (v.where()) {
    case value::location::reg: {
        __builtin_unreachable();
    }
    case value::location::stack: {
        auto offset = v.as<uint32_t>();
        ldr_offset(code, offset, stackreg, reg);
        return reg;
    }
    case value::location::imm:
    case value::location::flags:
        __builtin_unreachable();
    }

    assert(false);
}

bool Arm64::move_results(std::byte *&code, WasmStack &stack,
                         ControlFlow &flow) {
    auto arity = flow.expected.size();
    auto resultless_stack = stack.sp() - flow.expected.bytesize();

    // if the results are already in the right place, we don't need to move them
    if (arity == 0 /* || resultless_stack == flow.stack_offset */)
        return false;

    // stack.sp() doesn't include locals
    auto local_bytes = stack_size - stack.sp();
    auto stack_offset = local_bytes + flow.stack_offset;
    auto stack_iter = stack.rbegin();

    std::optional<ireg> intreg = std::nullopt;
    std::optional<freg> floatreg = std::nullopt;

    auto expected = values - arity;
    for (int i = 0; i < arity; i++) {
        auto v = flow.expected[i];
        if (v == valtype::f32 || v == valtype::f64) {
            auto reg = adapt_value_into(code, expected[i], floatreg);
            str_offset(code, stack_offset, stackreg, reg);
            stack_offset += sizeof(runtime::WasmValue);
        } else {
            auto reg = adapt_value_into(code, expected[i], intreg);
            str_offset(code, stack_offset, stackreg, reg);
            stack_offset += sizeof(runtime::WasmValue);
        }

        stack_iter++;
    }

    auto discarded =
        (resultless_stack - flow.stack_offset) / sizeof(runtime::WasmValue);

    for (auto i = 0; i < discarded; i++) {
        drop(code, stack, *stack_iter);
        stack_iter++;
    }

    return true;
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
        ((ret[I] =
              adapt_value<std::tuple_element_t<I, Params>>(code, values[I])),
         ...);
    }(std::make_index_sequence<nparams>{});

    if constexpr (std::is_same_v<Result, iwant::ireg>) {
        ret.back() = value::reg(intregs.result(code));
    } else if constexpr (std::is_same_v<Result, iwant::freg>) {
        ret.back() = value::reg(floatregs.result(code));
    } else {
        static_assert(std::is_same_v<Result, iwant::none>);
    }

    return ret;
}

template <typename... Args>
void Arm64::finalize(std::byte *&code, Args... results) {
    static_assert(sizeof...(Args) > 0);

    auto finalize = [&](auto result) {
        if constexpr (std::is_same_v<decltype(result), ireg>)
            intregs.claim(result,
                          decltype(intregs)::metadata(code, stack_size));
        else
            floatregs.claim(result,
                            decltype(floatregs)::metadata(code, stack_size));
        // buffer area for spilling
        put(code, noop);

        push(value::reg(result));
    };

    (finalize(results), ...);

    if constexpr ((std::is_same_v<Args, ireg> || ...))
        intregs.commit();
    if constexpr ((std::is_same_v<Args, freg> || ...))
        floatregs.commit();
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

    stack_size = fn.locals.size() * sizeof(runtime::WasmValue);
}
void Arm64::exit_function(SHARED_PARAMS, ControlFlow &flow) {
    // note: this has to be fixed to not potentially overwrite locals
    // that are being returned

    auto &fn = std::get<Function>(flow.construct).fn;

    clobber_flags(code);

    auto local_bytes = fn.locals.bytesize();

    if (!stack.polymorphism()) {
        values -= fn.type.results.size();
        for (auto i = 0; i < fn.type.results.size(); i++) {
            auto result = fn.type.results[i];
            auto offset = local_bytes + i * sizeof(runtime::WasmValue);

            // if the value is already there, skip it
            if (values[i].is<value::location::stack>() &&
                values[i].as<uint32_t>() == offset)
                continue;

            if (result == valtype::i32 || result == valtype::i64 ||
                result == valtype::funcref || result == valtype::externref) {
                str_offset(
                    code, offset, stackreg,
                    adapt_value<iwant::ireg>(code, values[i]).as<ireg>());
            } else if (result == valtype::f32 || result == valtype::f64) {
                str_offset(
                    code, offset, stackreg,
                    adapt_value<iwant::freg>(code, values[i]).as<freg>());
            }
        }
    }

    // i want stuff to jump here, because this is where callee saved registers
    // are restored, and results are copied
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

    // restore saved values
    for (auto i = 0; i < fn.locals.size(); i++) {
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

    // return values should be in [local_bytes, ...), so copy them backwards
    // into the local area
    // clobber at will (in this case x3)
    for (auto i = 0; i < fn.type.results.size(); i++) {
        auto result = fn.type.results[i];
        auto final_offset = i * sizeof(runtime::WasmValue);
        auto current_offset = local_bytes + final_offset;

        ldr_offset(code, current_offset, stackreg, ireg::x3);
        str_offset(code, final_offset, stackreg, ireg::x3);
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
void Arm64::block(SHARED_PARAMS, WasmSignature &sig) {}
void Arm64::loop(SHARED_PARAMS, WasmSignature &sig) {}
std::byte *Arm64::if_(SHARED_PARAMS, WasmSignature &sig) {}
std::byte *Arm64::else_(SHARED_PARAMS, WasmSignature &sig,
                        std::byte *if_location) {}
void Arm64::end(SHARED_PARAMS, ControlFlow &flow) {
    if (std::holds_alternative<If>(flow.construct)) {
        amend_br(std::get<If>(flow.construct).else_jump, code);
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

    move_results(code, stack, flow);

    values -= flow.expected.size();
    stack_size -= flow.expected.bytesize();

    auto imm = code;
    b(code, 0);

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
    if (move_results(code, stack, flow)) {
        imm = code;
        b(code, 0);

        auto jump = code - condjump;
        if (condition.is<value::location::flags>()) {
            bcond(condjump, jump, invert(condition.as<cond>()));
        } else {
            cbz(condjump, false, jump, condition.as<ireg>());
        }
    } else {
        imm = condjump;
        if (condition.is<value::location::flags>()) {
            bcond(condjump, 0, condition.as<cond>());
        } else {
            cbnz(condjump, false, 0, condition.as<ireg>());
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
                     std::span<uint32_t> targets) {}
void Arm64::return_(SHARED_PARAMS, std::span<ControlFlow> control_stack) {
    br(code, stack, control_stack, control_stack.size() - 1);
}
void Arm64::call(SHARED_PARAMS, FunctionShell &fn, uint32_t func_offset) {}
void Arm64::call_indirect(SHARED_PARAMS, uint32_t table_offset,
                          WasmSignature &type) {}
void Arm64::drop(SHARED_PARAMS, valtype type) {
    values--;
    stack_size -= sizeof(runtime::WasmValue);

    switch (values->where()) {
    case value::location::reg:
        if (type == valtype::f32 || type == valtype::f64)
            floatregs.surrender(values->as<freg>());
        else
            intregs.surrender(values->as<ireg>());
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
void Arm64::select(SHARED_PARAMS, valtype type) {}
void Arm64::select_t(SHARED_PARAMS, valtype type) {}
void Arm64::localget(SHARED_PARAMS, FunctionShell &fn, uint32_t local_idx) {
    push(locals[local_idx]);
}
void Arm64::localset(SHARED_PARAMS, FunctionShell &fn, uint32_t local_idx) {
    auto ty = fn.locals[local_idx];
    auto local = locals[local_idx];

    if (local.is<value::location::reg>()) {
        // i don't like that this specializes allocate_registers
        // but tbf it's the only place i need to do it

        intregs.begin();
        floatregs.begin();

        values -= 1;
        stack_size -= sizeof(runtime::WasmValue);

        if (ty == valtype::f32 || ty == valtype::f64) {
            if (values->is<value::location::reg>() &&
                is_volatile(values->as<freg>()) &&
                floatregs.check_spill(values->as<freg>(), code - sizeof(inst)))
                code -= sizeof(inst);

            auto reg = std::make_optional(local.as<freg>());
            auto v = adapt_value_into(code, values[0], reg);
            if (*reg != v)
                mov(code, v, *reg);
        } else {
            if (values->is<value::location::reg>() &&
                is_volatile(values->as<ireg>()) &&
                intregs.check_spill(values->as<ireg>(), code - sizeof(inst)))
                code -= sizeof(inst);

            auto reg = std::make_optional(local.as<ireg>());
            auto v = adapt_value_into(code, values[0], reg);
            if (*reg != v)
                mov(code, v, *reg);
        }
    } else {
        if (ty == valtype::f32 || ty == valtype::f64) {
            auto [reg] = allocate_registers<std::tuple<iwant::freg>>(code);
            str_offset(code, local.as<uint32_t>(), stackreg, reg.as<freg>());
        } else {
            auto [reg] = allocate_registers<std::tuple<iwant::ireg>>(code);
            str_offset(code, local.as<uint32_t>(), stackreg, reg.as<ireg>());
        }
    }
}
void Arm64::localtee(SHARED_PARAMS, FunctionShell &fn, uint32_t local_idx) {
    localset(code, stack, fn, local_idx);
    localget(code, stack, fn, local_idx);
}
void Arm64::tableget(SHARED_PARAMS, uint64_t misc_offset) {}
void Arm64::tableset(SHARED_PARAMS, uint64_t misc_offset) {}
void Arm64::globalget(SHARED_PARAMS, uint64_t misc_offset) {}
void Arm64::globalset(SHARED_PARAMS, uint64_t misc_offset) {}
void Arm64::memorysize(SHARED_PARAMS) {}
void Arm64::memorygrow(SHARED_PARAMS) {}
void Arm64::i32const(SHARED_PARAMS, uint32_t cons) { push(value::imm(cons)); }
void Arm64::i64const(SHARED_PARAMS, uint64_t cons) {
    if (cons <= std::numeric_limits<uint32_t>::max()) {
        auto [res] = allocate_registers<std::tuple<>, iwant::ireg>(code);
        mov(code, cons, res.as<ireg>());
        finalize(code, res.as<ireg>());
    } else {
        push(value::imm(cons));
    }
}
void Arm64::f32const(SHARED_PARAMS, float cons) {
    auto [res] = allocate_registers<std::tuple<>, iwant::freg>(code);
    auto temp = intregs.temporary(code);

    mov(code, std::bit_cast<uint32_t>(cons), temp);
    mov(code, temp, res.as<freg>());

    finalize(code, res.as<freg>());
}
void Arm64::f64const(SHARED_PARAMS, double cons) {
    auto [res] = allocate_registers<std::tuple<>, iwant::freg>(code);
    auto temp = intregs.temporary(code);

    mov(code, std::bit_cast<uint64_t>(cons), temp);
    mov(code, temp, res.as<freg>());

    finalize(code, res.as<freg>());
}
void Arm64::i32load(SHARED_PARAMS, uint64_t offset, uint64_t align) {}
void Arm64::i64load(SHARED_PARAMS, uint64_t offset, uint64_t align) {}
void Arm64::f32load(SHARED_PARAMS, uint64_t offset, uint64_t align) {}
void Arm64::f64load(SHARED_PARAMS, uint64_t offset, uint64_t align) {}
void Arm64::i32load8_s(SHARED_PARAMS, uint64_t offset, uint64_t align) {}
void Arm64::i32load8_u(SHARED_PARAMS, uint64_t offset, uint64_t align) {}
void Arm64::i32load16_s(SHARED_PARAMS, uint64_t offset, uint64_t align) {}
void Arm64::i32load16_u(SHARED_PARAMS, uint64_t offset, uint64_t align) {}
void Arm64::i64load8_s(SHARED_PARAMS, uint64_t offset, uint64_t align) {}
void Arm64::i64load8_u(SHARED_PARAMS, uint64_t offset, uint64_t align) {}
void Arm64::i64load16_s(SHARED_PARAMS, uint64_t offset, uint64_t align) {}
void Arm64::i64load16_u(SHARED_PARAMS, uint64_t offset, uint64_t align) {}
void Arm64::i64load32_s(SHARED_PARAMS, uint64_t offset, uint64_t align) {}
void Arm64::i64load32_u(SHARED_PARAMS, uint64_t offset, uint64_t align) {}
void Arm64::i32store(SHARED_PARAMS, uint64_t offset, uint64_t align) {}
void Arm64::i64store(SHARED_PARAMS, uint64_t offset, uint64_t align) {}
void Arm64::f32store(SHARED_PARAMS, uint64_t offset, uint64_t align) {}
void Arm64::f64store(SHARED_PARAMS, uint64_t offset, uint64_t align) {}
void Arm64::i32store8(SHARED_PARAMS, uint64_t offset, uint64_t align) {}
void Arm64::i32store16(SHARED_PARAMS, uint64_t offset, uint64_t align) {}
void Arm64::i64store8(SHARED_PARAMS, uint64_t offset, uint64_t align) {}
void Arm64::i64store16(SHARED_PARAMS, uint64_t offset, uint64_t align) {}
void Arm64::i64store32(SHARED_PARAMS, uint64_t offset, uint64_t align) {}
void Arm64::i32eqz(SHARED_PARAMS) {
    clobber_flags(code);

    auto [p] = allocate_registers<std::tuple<iwant::ireg>>(code);
    cmp(code, false, p.as<ireg>(), ireg::xzr);
    push(value::flag(cond::eq));
}
void Arm64::i64eqz(SHARED_PARAMS) {
    clobber_flags(code);

    auto [p] = allocate_registers<std::tuple<iwant::ireg>>(code);
    cmp(code, true, p.as<ireg>(), ireg::xzr);
    push(value::flag(cond::eq));
}

#define COMPARISON(is_64, op)                                                  \
    do {                                                                       \
        clobber_flags(code);                                                   \
        auto [p1, p2] = allocate_registers<                                    \
            std::tuple<iwant::ireg, iwant::literal<1 << 12>>>(code);           \
        if (p2.is<value::location::imm>()) {                                   \
            cmp(code, is_64, p1.as<ireg>(), p2.as<uint32_t>());                \
        } else {                                                               \
            cmp(code, is_64, p1.as<ireg>(), p2.as<ireg>());                    \
        }                                                                      \
        push(value::flag(cond::op));                                           \
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
        clobber_flags(code);                                                   \
        auto [p1, p2] =                                                        \
            allocate_registers<std::tuple<iwant::freg, iwant::freg>>(code);    \
        fcmp(code, is_64, p1.as<freg>(), p2.as<freg>());                       \
        push(value::flag(cond::op));                                           \
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

    clz(code, false, res.as<ireg>(), p1.as<ireg>());

    finalize(code, res.as<ireg>());
}
void Arm64::i64clz(SHARED_PARAMS) {}
void Arm64::i32ctz(SHARED_PARAMS) {}
void Arm64::i64ctz(SHARED_PARAMS) {}
void Arm64::i32popcnt(SHARED_PARAMS) {}
void Arm64::i64popcnt(SHARED_PARAMS) {}
void Arm64::i32add(SHARED_PARAMS) {
    auto [p1, p2, res] =
        allocate_registers<std::tuple<iwant::ireg, iwant::literal<1 << 12>>,
                           iwant::ireg>(code);

    if (p2.is<value::location::imm>()) {
        add(code, false, res.as<ireg>(), p1.as<ireg>(), p2.as<uint32_t>());
    } else {
        add(code, false, res.as<ireg>(), p1.as<ireg>(), p2.as<ireg>());
    }

    finalize(code, res.as<ireg>());
}
void Arm64::i64add(SHARED_PARAMS) {}
void Arm64::i32sub(SHARED_PARAMS) {
    auto [p1, p2, res] =
        allocate_registers<std::tuple<iwant::ireg, iwant::literal<1 << 12>>,
                           iwant::ireg>(code);

    if (p2.is<value::location::imm>()) {
        sub(code, false, res.as<ireg>(), p1.as<ireg>(), p2.as<uint32_t>());
    } else {
        sub(code, false, res.as<ireg>(), p1.as<ireg>(), p2.as<ireg>());
    }

    finalize(code, res.as<ireg>());
}
void Arm64::i64sub(SHARED_PARAMS) {}
void Arm64::i32mul(SHARED_PARAMS) {}
void Arm64::i64mul(SHARED_PARAMS) {}
void Arm64::i32div_s(SHARED_PARAMS) {}
void Arm64::i64div_s(SHARED_PARAMS) {}
void Arm64::i32div_u(SHARED_PARAMS) {}
void Arm64::i64div_u(SHARED_PARAMS) {}
void Arm64::i32rem_s(SHARED_PARAMS) {}
void Arm64::i64rem_s(SHARED_PARAMS) {}
void Arm64::i32rem_u(SHARED_PARAMS) {}
void Arm64::i64rem_u(SHARED_PARAMS) {}
void Arm64::i32and(SHARED_PARAMS) {}
void Arm64::i64and(SHARED_PARAMS) {}
void Arm64::i32or(SHARED_PARAMS) {}
void Arm64::i64or(SHARED_PARAMS) {}
void Arm64::i32xor(SHARED_PARAMS) {}
void Arm64::i64xor(SHARED_PARAMS) {}
void Arm64::i32shl(SHARED_PARAMS) {}
void Arm64::i64shl(SHARED_PARAMS) {}
void Arm64::i32shr_s(SHARED_PARAMS) {}
void Arm64::i64shr_s(SHARED_PARAMS) {}
void Arm64::i32shr_u(SHARED_PARAMS) {}
void Arm64::i64shr_u(SHARED_PARAMS) {}
void Arm64::i32rotl(SHARED_PARAMS) {}
void Arm64::i64rotl(SHARED_PARAMS) {}
void Arm64::i32rotr(SHARED_PARAMS) {}
void Arm64::i64rotr(SHARED_PARAMS) {}
void Arm64::f32abs(SHARED_PARAMS) {}
void Arm64::f64abs(SHARED_PARAMS) {}
void Arm64::f32neg(SHARED_PARAMS) {}
void Arm64::f64neg(SHARED_PARAMS) {}
void Arm64::f32ceil(SHARED_PARAMS) {}
void Arm64::f64ceil(SHARED_PARAMS) {}
void Arm64::f32floor(SHARED_PARAMS) {}
void Arm64::f64floor(SHARED_PARAMS) {}
void Arm64::f32trunc(SHARED_PARAMS) {}
void Arm64::f64trunc(SHARED_PARAMS) {}
void Arm64::f32nearest(SHARED_PARAMS) {}
void Arm64::f64nearest(SHARED_PARAMS) {}
void Arm64::f32sqrt(SHARED_PARAMS) {}
void Arm64::f64sqrt(SHARED_PARAMS) {}
void Arm64::f32add(SHARED_PARAMS) {}
void Arm64::f64add(SHARED_PARAMS) {}
void Arm64::f32sub(SHARED_PARAMS) {}
void Arm64::f64sub(SHARED_PARAMS) {}
void Arm64::f32mul(SHARED_PARAMS) {}
void Arm64::f64mul(SHARED_PARAMS) {}
void Arm64::f32div(SHARED_PARAMS) {}
void Arm64::f64div(SHARED_PARAMS) {}
void Arm64::f32min(SHARED_PARAMS) {}
void Arm64::f64min(SHARED_PARAMS) {}
void Arm64::f32max(SHARED_PARAMS) {}
void Arm64::f64max(SHARED_PARAMS) {}
void Arm64::f32copysign(SHARED_PARAMS) {}
void Arm64::f64copysign(SHARED_PARAMS) {}
void Arm64::i32wrap_i64(SHARED_PARAMS) {}
void Arm64::i64extend_i32_s(SHARED_PARAMS) {}
void Arm64::i64extend_i32_u(SHARED_PARAMS) {}
void Arm64::i32trunc_f32_s(SHARED_PARAMS) {}
void Arm64::i64trunc_f32_s(SHARED_PARAMS) {}
void Arm64::i32trunc_f32_u(SHARED_PARAMS) {}
void Arm64::i64trunc_f32_u(SHARED_PARAMS) {}
void Arm64::i32trunc_f64_s(SHARED_PARAMS) {}
void Arm64::i64trunc_f64_s(SHARED_PARAMS) {}
void Arm64::i32trunc_f64_u(SHARED_PARAMS) {}
void Arm64::i64trunc_f64_u(SHARED_PARAMS) {}
void Arm64::f32convert_i32_s(SHARED_PARAMS) {}
void Arm64::f64convert_i32_s(SHARED_PARAMS) {}
void Arm64::f32convert_i32_u(SHARED_PARAMS) {}
void Arm64::f64convert_i32_u(SHARED_PARAMS) {}
void Arm64::f32convert_i64_s(SHARED_PARAMS) {}
void Arm64::f64convert_i64_s(SHARED_PARAMS) {}
void Arm64::f32convert_i64_u(SHARED_PARAMS) {}
void Arm64::f64convert_i64_u(SHARED_PARAMS) {}
void Arm64::f32demote_f64(SHARED_PARAMS) {}
void Arm64::f64promote_f32(SHARED_PARAMS) {}
void Arm64::i32reinterpret_f32(SHARED_PARAMS) {}
void Arm64::f32reinterpret_i32(SHARED_PARAMS) {}
void Arm64::i64reinterpret_f64(SHARED_PARAMS) {}
void Arm64::f64reinterpret_i64(SHARED_PARAMS) {}
void Arm64::i32extend8_s(SHARED_PARAMS) {}
void Arm64::i32extend16_s(SHARED_PARAMS) {}
void Arm64::i64extend8_s(SHARED_PARAMS) {}
void Arm64::i64extend16_s(SHARED_PARAMS) {}
void Arm64::i64extend32_s(SHARED_PARAMS) {}
void Arm64::ref_null(SHARED_PARAMS) {}
void Arm64::ref_is_null(SHARED_PARAMS) {}
void Arm64::ref_func(SHARED_PARAMS, uint64_t misc_offset) {}
void Arm64::ref_eq(SHARED_PARAMS) {}
void Arm64::i32_trunc_sat_f32_s(SHARED_PARAMS) {}
void Arm64::i32_trunc_sat_f32_u(SHARED_PARAMS) {}
void Arm64::i32_trunc_sat_f64_s(SHARED_PARAMS) {}
void Arm64::i32_trunc_sat_f64_u(SHARED_PARAMS) {}
void Arm64::i64_trunc_sat_f32_s(SHARED_PARAMS) {}
void Arm64::i64_trunc_sat_f32_u(SHARED_PARAMS) {}
void Arm64::i64_trunc_sat_f64_s(SHARED_PARAMS) {}
void Arm64::i64_trunc_sat_f64_u(SHARED_PARAMS) {}
void Arm64::memory_init(SHARED_PARAMS, uint64_t misc_offset) {}
void Arm64::data_drop(SHARED_PARAMS, uint64_t misc_offset) {}
void Arm64::memory_copy(SHARED_PARAMS) {}
void Arm64::memory_fill(SHARED_PARAMS) {}
void Arm64::table_init(SHARED_PARAMS, uint64_t seg_offset,
                       uint64_t table_offset) {}
void Arm64::elem_drop(SHARED_PARAMS, uint64_t misc_offset) {}
void Arm64::table_copy(SHARED_PARAMS, uint64_t dst_offset,
                       uint64_t src_offset) {}
void Arm64::table_grow(SHARED_PARAMS, uint64_t misc_offset) {}
void Arm64::table_size(SHARED_PARAMS, uint64_t misc_offset) {}
void Arm64::table_fill(SHARED_PARAMS, uint64_t misc_offset) {}

} // namespace arm64
} // namespace mitey