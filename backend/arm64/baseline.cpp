#include "./baseline.hpp"
#include <cassert>
#include <cstring>
#include <limits>
#include <optional>
#include <tuple>

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

void add(std::byte *&code, bool sf, bool shift, uint16_t imm12, ireg rn,
         ireg rd) {
    addsub(code, sf, false, false, shift, imm12, rn, rd);
}

void add(std::byte *&code, bool sf, ireg rm, ireg rn, ireg rd,
         shifttype shift = shifttype::lsl, uint8_t shift_n = 0) {
    addsub(code, sf, false, false, shift, rm, shift_n, rn, rd);
}

void sub(std::byte *&code, bool sf, bool shift, uint16_t imm12, ireg rn,
         ireg rd) {
    addsub(code, sf, true, false, shift, imm12, rn, rd);
}

void subs(std::byte *&code, bool sf, bool shift, uint16_t imm12, ireg rn,
          ireg rd) {
    addsub(code, sf, true, true, shift, imm12, rn, rd);
}

void sub(std::byte *&code, bool sf, ireg rm, ireg rn, ireg rd,
         shifttype shift = shifttype::lsl, uint8_t shift_n = 0) {
    addsub(code, sf, true, false, shift, rm, shift_n, rn, rd);
}

void subs(std::byte *&code, bool sf, ireg rm, ireg rn, ireg rd,
          shifttype shift = shifttype::lsl, uint8_t shift_n = 0) {
    addsub(code, sf, true, true, shift, rm, shift_n, rn, rd);
}

void cmp(std::byte *&code, bool sf, ireg rm, ireg rn,
         shifttype shift = shifttype::lsl, uint8_t shift_n = 0) {
    subs(code, sf, rm, rn, ireg::xzr, shift, shift_n);
}

void cmp(std::byte *&code, bool sf, bool shift, uint16_t imm12, ireg rn) {
    subs(code, sf, shift, imm12, rn, ireg::xzr);
}

void clz(std::byte *&code, bool sf, ireg rn, ireg rd) {
    put(code, 0b0101101011000000000100000000000 |
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
    // todo: should this have a fast path for 0?
    // i think that should be handled elsewhere
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
    // consts don't occupy stack space
    if (!v.is<value::location::imm>()) {
        stack_size += sizeof(runtime::WasmValue);
    }
}

template <typename To> value Arm64::adapt_value(std::byte *&code, value v) {
    using RegType =
        std::conditional_t<std::is_same_v<To, iwant::freg>, freg, ireg>;

    switch (v.where()) {
    case value::location::reg: {
        stack_size -= sizeof(runtime::WasmValue);

        if (is_volatile(v.as<RegType>())) {
            RegType reg;
            if constexpr (std::is_same_v<To, iwant::freg>)
                reg = floatregs.temporary(code);
            else
                reg = intregs.temporary(code);
            mov(code, v.as<RegType>(), reg);
            return value::reg(reg);
        } else {
            return v;
        }
    }
    case value::location::stack: {
        stack_size -= sizeof(runtime::WasmValue);

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
        if (imm < To::threshold) {
            return v;
        } else if (auto mask = tryLogicalImm(imm);
                   std::is_same_v<To, iwant::bitmask> && mask) {
            return value::imm(std::bit_cast<uint32_t>(*mask));
        } else {
            auto reg = intregs.temporary(code);
            mov(code, imm, reg);
            return value::reg(reg);
        }
    }
    case value::location::flag: {
        auto better_not = !std::is_same_v<To, iwant::freg>;
        assert(better_not);

        stack_size -= sizeof(runtime::WasmValue);

        if (std::is_same_v<To, iwant::flags>) {
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

    intregs.commit();
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
void Arm64::exit_function(SHARED_PARAMS, FunctionShell &fn) {
    // note: this has to be fixed to not potentially overwrite locals
    // that are being returned

    clobber_flags(code);
    clobber_registers(code);

    // restore saved values
    for (auto i = 0; i < fn.type.params.size(); i++) {
        if (!locals[i].is<value::location::reg>())
            continue;

        auto param = fn.locals[i];
        auto offset = i * sizeof(runtime::WasmValue);

        if (param == valtype::i32 || param == valtype::i64 ||
            param == valtype::funcref || param == valtype::externref) {
            ldr_offset(code, offset, stackreg, locals[i].as<ireg>());
        } else if (param == valtype::f32 || param == valtype::f64) {
            ldr_offset(code, offset, stackreg, locals[i].as<freg>());
        }
    }

    values -= fn.type.results.size();
    for (auto i = 0; i < fn.type.results.size(); i++) {
        auto result = fn.type.results[i];
        auto offset = i * sizeof(runtime::WasmValue);

        if (result == valtype::i32 || result == valtype::i64 ||
            result == valtype::funcref || result == valtype::externref) {
            str_offset(code, offset, stackreg,
                       adapt_value<iwant::ireg>(code, values[i]).as<ireg>());
        } else if (result == valtype::f32 || result == valtype::f64) {
            str_offset(code, offset, stackreg,
                       adapt_value<iwant::freg>(code, values[i]).as<freg>());
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
        auto [res] = allocate_registers<std::tuple<>, iwant::freg>(code);
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
void Arm64::i32clz(SHARED_PARAMS) {
    auto [p1, res] =
        allocate_registers<std::tuple<iwant::ireg>, iwant::ireg>(code);

    clz(code, false, p1.as<ireg>(), res.as<ireg>());

    finalize(code, res.as<ireg>());
}
// void Arm64::i64clz(SHARED_PARAMS);
// void Arm64::i32ctz(SHARED_PARAMS);
// void Arm64::i64ctz(SHARED_PARAMS);
// void Arm64::i32popcnt(SHARED_PARAMS);
// void Arm64::i64popcnt(SHARED_PARAMS);
void Arm64::i32add(SHARED_PARAMS) {
    auto [p1, p2, res] =
        allocate_registers<std::tuple<iwant::ireg, iwant::literal<1 << 12>>,
                           iwant::ireg>(code);

    if (p2.is<value::location::imm>()) {
        add(code, false, false, p2.as<uint32_t>(), p1.as<ireg>(),
            res.as<ireg>());
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