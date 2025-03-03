#pragma once

#include "../../module.hpp"
#include "./enums.hpp"
#include "./reg_lru.hpp"
#include <cstddef>
#include <cstdint>
#include <span>

namespace mitey {
namespace arm64 {

class value {
  public:
    enum class location { reg, stack, imm, flag };

  private:
    location loc;
    uint32_t val;

    value(location loc, uint32_t val) : loc(loc), val(val) {}

  public:
    value() = default;

    static value reg(ireg reg) {
        return value(location::reg, static_cast<uint32_t>(reg));
    }
    static value reg(freg reg) {
        return value(location::reg, static_cast<uint32_t>(reg));
    }
    static value stack(uint32_t offset) {
        return value(location::stack, offset);
    }
    static value imm(uint32_t val) { return value(location::imm, val); }
    static value flag(cond c) {
        return value(location::flag, static_cast<uint32_t>(c));
    }

    template <typename T> T as() const { return static_cast<T>(val); }
    location where() const { return loc; }
    template <location loc> bool is() const { return where() == loc; }
};

#define SHARED_PARAMS std::byte *&code, WasmStack &stack

class Arm64 {
    template <typename RegType, size_t First, size_t Last> class reg_manager {
      public:
        struct metadata {
            // when a register is inactive, this is a null pointer
            // when a register is given a value, this is set to a noop
            // when a register is stolen, this is set to a spill
            std::byte *spilladdr = nullptr;
            uint32_t stack_offset = 0;
        };

      private:
        reg_lru<Last - First> regs;
        metadata data[Last - First];

        void spill(uint8_t reg) {
            auto [addr, offset] = data[reg - First];
            str_offset(addr, offset, stackreg, reg);
        }

        uint8_t to_index(RegType reg) {
            return static_cast<uint8_t>(reg) - First;
        }

        RegType from_index(uint8_t idx) {
            return static_cast<RegType>(idx + First);
        }

      public:
        std::pair<RegType, metadata *> steal(std::byte *&code) {
            auto idx = regs.steal();
            auto reg = static_cast<RegType>(idx + First);
            return {reg, &data[idx]};
        }

        // assumes the register is dead
        void claim(RegType reg, metadata meta) {
            auto idx = to_index(reg);
            regs.access(idx);
            data[idx] = meta;
        }

        void surrender(RegType reg) {
            auto idx = to_index(reg);
            regs.discard(idx);
            data[idx] = metadata(nullptr, 0);
        }
    };

    // callee saved registers
    std::span<value> locals;
    // caller saved registers
    reg_manager<ireg, 3, icaller_saved.size()> intregs;
    reg_manager<freg, 0, fcaller_saved.size()> floatregs;

    struct flags {
        // offset to spill into
        uint32_t stack_offset = 0;
        // pointer into values pointing to flag value (or nil)
        value *val = nullptr;
    };
    flags flag;

    uint32_t stack_size = 0;
    value *values = values_start.get();
    std::unique_ptr<value[]> values_start = std::make_unique<value[]>(65536);

    void clobber_flags(std::byte *&code);
    void clobber_registers(std::byte *&code);

    void push(value v);

    struct poption {
      private:
        // literal/bitmask/flags implicitly accept ireg
        enum class canbe { ireg, freg, literal, bitmask, flags, none };

      public:
        struct type {
            canbe val;
            uint32_t threshold;

            bool operator==(type c) const { return val == c.val; }
            bool fits(uint32_t t) const { return t < threshold; }
        };

        static constexpr auto none = type(canbe::none, 0);
        static constexpr auto ireg = type(canbe::ireg, 0);
        static constexpr auto freg = type(canbe::freg, 0);
        template <uint32_t t>
        static constexpr auto literal = type(canbe::literal, t);
        static constexpr auto bitmask = type(canbe::bitmask, 0);
        static constexpr auto flags = type(canbe::flags, 0);
    };
    // eventually i'll have to support multivalue here
    template <size_t nparams>
    std::array<value, nparams + 1>
    allocate_registers(std::byte *&code,
                       std::array<poption::type, nparams> params,
                       poption::type result);

    void finalize(std::byte *&code, ireg result);
    void finalize(std::byte *&code, freg result);

  public:
    // todo: figure out what values for these
    static constexpr size_t function_overhead = 100 * sizeof(uint32_t);
    static constexpr size_t max_instruction = 100 * sizeof(uint32_t);

    void start_function(SHARED_PARAMS, FunctionShell &fn);
    void exit_function(SHARED_PARAMS, FunctionShell &fn);

    void unreachable(SHARED_PARAMS);
    void nop(SHARED_PARAMS);
    void block(SHARED_PARAMS, WasmSignature &sig);
    void loop(SHARED_PARAMS, WasmSignature &sig);
    std::byte *if_(SHARED_PARAMS, WasmSignature &sig);
    std::byte *else_(SHARED_PARAMS, WasmSignature &sig, std::byte *if_location);
    void end(SHARED_PARAMS, ControlFlow &flow);
    void br(SHARED_PARAMS, std::span<ControlFlow> control_stack,
            uint32_t depth);
    void br_if(SHARED_PARAMS, std::span<ControlFlow> control_stack,
               uint32_t depth);
    void br_table(SHARED_PARAMS, std::span<ControlFlow> control_stack,
                  std::span<uint32_t> targets);
    void return_(SHARED_PARAMS, std::span<ControlFlow> control_stack);
    void call(SHARED_PARAMS, FunctionShell &fn, uint32_t func_offset);
    void call_indirect(SHARED_PARAMS, uint32_t table_offset,
                       WasmSignature &type);
    void drop(SHARED_PARAMS);
    void select(SHARED_PARAMS);
    void select_t(SHARED_PARAMS);
    void localget(SHARED_PARAMS, FunctionShell &fn, uint32_t local_idx);
    void localset(SHARED_PARAMS, FunctionShell &fn, uint32_t local_idx);
    void localtee(SHARED_PARAMS, FunctionShell &fn, uint32_t local_idx);
    void tableget(SHARED_PARAMS, uint64_t misc_offset);
    void tableset(SHARED_PARAMS, uint64_t misc_offset);
    void globalget(SHARED_PARAMS, uint64_t misc_offset);
    void globalset(SHARED_PARAMS, uint64_t misc_offset);
    void memorysize(SHARED_PARAMS);
    void memorygrow(SHARED_PARAMS);
    void i32const(SHARED_PARAMS, uint32_t cons);
    void i64const(SHARED_PARAMS, uint64_t cons);
    void f32const(SHARED_PARAMS, float cons);
    void f64const(SHARED_PARAMS, double cons);
    void i32load(SHARED_PARAMS, uint64_t offset, uint64_t align);
    void i64load(SHARED_PARAMS, uint64_t offset, uint64_t align);
    void f32load(SHARED_PARAMS, uint64_t offset, uint64_t align);
    void f64load(SHARED_PARAMS, uint64_t offset, uint64_t align);
    void i32load8_s(SHARED_PARAMS, uint64_t offset, uint64_t align);
    void i32load8_u(SHARED_PARAMS, uint64_t offset, uint64_t align);
    void i32load16_s(SHARED_PARAMS, uint64_t offset, uint64_t align);
    void i32load16_u(SHARED_PARAMS, uint64_t offset, uint64_t align);
    void i64load8_s(SHARED_PARAMS, uint64_t offset, uint64_t align);
    void i64load8_u(SHARED_PARAMS, uint64_t offset, uint64_t align);
    void i64load16_s(SHARED_PARAMS, uint64_t offset, uint64_t align);
    void i64load16_u(SHARED_PARAMS, uint64_t offset, uint64_t align);
    void i64load32_s(SHARED_PARAMS, uint64_t offset, uint64_t align);
    void i64load32_u(SHARED_PARAMS, uint64_t offset, uint64_t align);
    void i32store(SHARED_PARAMS, uint64_t offset, uint64_t align);
    void i64store(SHARED_PARAMS, uint64_t offset, uint64_t align);
    void f32store(SHARED_PARAMS, uint64_t offset, uint64_t align);
    void f64store(SHARED_PARAMS, uint64_t offset, uint64_t align);
    void i32store8(SHARED_PARAMS, uint64_t offset, uint64_t align);
    void i32store16(SHARED_PARAMS, uint64_t offset, uint64_t align);
    void i64store8(SHARED_PARAMS, uint64_t offset, uint64_t align);
    void i64store16(SHARED_PARAMS, uint64_t offset, uint64_t align);
    void i64store32(SHARED_PARAMS, uint64_t offset, uint64_t align);
    void i32eqz(SHARED_PARAMS);
    void i64eqz(SHARED_PARAMS);
    void i32eq(SHARED_PARAMS);
    void i64eq(SHARED_PARAMS);
    void i32ne(SHARED_PARAMS);
    void i64ne(SHARED_PARAMS);
    void i32lt_s(SHARED_PARAMS);
    void i64lt_s(SHARED_PARAMS);
    void i32lt_u(SHARED_PARAMS);
    void i64lt_u(SHARED_PARAMS);
    void i32gt_s(SHARED_PARAMS);
    void i64gt_s(SHARED_PARAMS);
    void i32gt_u(SHARED_PARAMS);
    void i64gt_u(SHARED_PARAMS);
    void i32le_s(SHARED_PARAMS);
    void i64le_s(SHARED_PARAMS);
    void i32le_u(SHARED_PARAMS);
    void i64le_u(SHARED_PARAMS);
    void i32ge_s(SHARED_PARAMS);
    void i64ge_s(SHARED_PARAMS);
    void i32ge_u(SHARED_PARAMS);
    void i64ge_u(SHARED_PARAMS);
    void f32eq(SHARED_PARAMS);
    void f64eq(SHARED_PARAMS);
    void f32ne(SHARED_PARAMS);
    void f64ne(SHARED_PARAMS);
    void f32lt(SHARED_PARAMS);
    void f64lt(SHARED_PARAMS);
    void f32gt(SHARED_PARAMS);
    void f64gt(SHARED_PARAMS);
    void f32le(SHARED_PARAMS);
    void f64le(SHARED_PARAMS);
    void f32ge(SHARED_PARAMS);
    void f64ge(SHARED_PARAMS);
    void i32clz(SHARED_PARAMS);
    void i64clz(SHARED_PARAMS);
    void i32ctz(SHARED_PARAMS);
    void i64ctz(SHARED_PARAMS);
    void i32popcnt(SHARED_PARAMS);
    void i64popcnt(SHARED_PARAMS);
    void i32add(SHARED_PARAMS);
    void i64add(SHARED_PARAMS);
    void i32sub(SHARED_PARAMS);
    void i64sub(SHARED_PARAMS);
    void i32mul(SHARED_PARAMS);
    void i64mul(SHARED_PARAMS);
    void i32div_s(SHARED_PARAMS);
    void i64div_s(SHARED_PARAMS);
    void i32div_u(SHARED_PARAMS);
    void i64div_u(SHARED_PARAMS);
    void i32rem_s(SHARED_PARAMS);
    void i64rem_s(SHARED_PARAMS);
    void i32rem_u(SHARED_PARAMS);
    void i64rem_u(SHARED_PARAMS);
    void i32and(SHARED_PARAMS);
    void i64and(SHARED_PARAMS);
    void i32or(SHARED_PARAMS);
    void i64or(SHARED_PARAMS);
    void i32xor(SHARED_PARAMS);
    void i64xor(SHARED_PARAMS);
    void i32shl(SHARED_PARAMS);
    void i64shl(SHARED_PARAMS);
    void i32shr_s(SHARED_PARAMS);
    void i64shr_s(SHARED_PARAMS);
    void i32shr_u(SHARED_PARAMS);
    void i64shr_u(SHARED_PARAMS);
    void i32rotl(SHARED_PARAMS);
    void i64rotl(SHARED_PARAMS);
    void i32rotr(SHARED_PARAMS);
    void i64rotr(SHARED_PARAMS);
    void f32abs(SHARED_PARAMS);
    void f64abs(SHARED_PARAMS);
    void f32neg(SHARED_PARAMS);
    void f64neg(SHARED_PARAMS);
    void f32ceil(SHARED_PARAMS);
    void f64ceil(SHARED_PARAMS);
    void f32floor(SHARED_PARAMS);
    void f64floor(SHARED_PARAMS);
    void f32trunc(SHARED_PARAMS);
    void f64trunc(SHARED_PARAMS);
    void f32nearest(SHARED_PARAMS);
    void f64nearest(SHARED_PARAMS);
    void f32sqrt(SHARED_PARAMS);
    void f64sqrt(SHARED_PARAMS);
    void f32add(SHARED_PARAMS);
    void f64add(SHARED_PARAMS);
    void f32sub(SHARED_PARAMS);
    void f64sub(SHARED_PARAMS);
    void f32mul(SHARED_PARAMS);
    void f64mul(SHARED_PARAMS);
    void f32div(SHARED_PARAMS);
    void f64div(SHARED_PARAMS);
    void f32min(SHARED_PARAMS);
    void f64min(SHARED_PARAMS);
    void f32max(SHARED_PARAMS);
    void f64max(SHARED_PARAMS);
    void f32copysign(SHARED_PARAMS);
    void f64copysign(SHARED_PARAMS);
    void i32wrap_i64(SHARED_PARAMS);
    void i64extend_i32_s(SHARED_PARAMS);
    void i64extend_i32_u(SHARED_PARAMS);
    void i32trunc_f32_s(SHARED_PARAMS);
    void i64trunc_f32_s(SHARED_PARAMS);
    void i32trunc_f32_u(SHARED_PARAMS);
    void i64trunc_f32_u(SHARED_PARAMS);
    void i32trunc_f64_s(SHARED_PARAMS);
    void i64trunc_f64_s(SHARED_PARAMS);
    void i32trunc_f64_u(SHARED_PARAMS);
    void i64trunc_f64_u(SHARED_PARAMS);
    void f32convert_i32_s(SHARED_PARAMS);
    void f64convert_i32_s(SHARED_PARAMS);
    void f32convert_i32_u(SHARED_PARAMS);
    void f64convert_i32_u(SHARED_PARAMS);
    void f32convert_i64_s(SHARED_PARAMS);
    void f64convert_i64_s(SHARED_PARAMS);
    void f32convert_i64_u(SHARED_PARAMS);
    void f64convert_i64_u(SHARED_PARAMS);
    void f32demote_f64(SHARED_PARAMS);
    void f64promote_f32(SHARED_PARAMS);
    void i32reinterpret_f32(SHARED_PARAMS);
    void f32reinterpret_i32(SHARED_PARAMS);
    void i64reinterpret_f64(SHARED_PARAMS);
    void f64reinterpret_i64(SHARED_PARAMS);
    void i32extend8_s(SHARED_PARAMS);
    void i32extend16_s(SHARED_PARAMS);
    void i64extend8_s(SHARED_PARAMS);
    void i64extend16_s(SHARED_PARAMS);
    void i64extend32_s(SHARED_PARAMS);
    void ref_null(SHARED_PARAMS);
    void ref_is_null(SHARED_PARAMS);
    void ref_func(SHARED_PARAMS, uint64_t misc_offset);
    void ref_eq(SHARED_PARAMS);
    void i32_trunc_sat_f32_s(SHARED_PARAMS);
    void i32_trunc_sat_f32_u(SHARED_PARAMS);
    void i32_trunc_sat_f64_s(SHARED_PARAMS);
    void i32_trunc_sat_f64_u(SHARED_PARAMS);
    void i64_trunc_sat_f32_s(SHARED_PARAMS);
    void i64_trunc_sat_f32_u(SHARED_PARAMS);
    void i64_trunc_sat_f64_s(SHARED_PARAMS);
    void i64_trunc_sat_f64_u(SHARED_PARAMS);
    void memory_init(SHARED_PARAMS, uint64_t misc_offset);
    void data_drop(SHARED_PARAMS, uint64_t misc_offset);
    void memory_copy(SHARED_PARAMS);
    void memory_fill(SHARED_PARAMS);
    void table_init(SHARED_PARAMS, uint64_t seg_offset, uint64_t table_offset);
    void elem_drop(SHARED_PARAMS, uint64_t misc_offset);
    void table_copy(SHARED_PARAMS, uint64_t dst_offset, uint64_t src_offset);
    void table_grow(SHARED_PARAMS, uint64_t misc_offset);
    void table_size(SHARED_PARAMS, uint64_t misc_offset);
    void table_fill(SHARED_PARAMS, uint64_t misc_offset);
};

// locals go in callee saved registers, because we don't want to save them
// around every call
// stack scratch can probably go in caller saved registers, because there's
// probably less of them at callsites

// some basics:
// instead of just tracking type, validation stack also tracks:
// - location (register name or stack)
//   - stack offset is implicit, even variables in registers are "on the stack"
//     for easier spills
//   - note register name can be a local
// - whether or not it's a const, if so its value
// - whether or not it's a flag
//   - note there can't be multiple flags in the stack at once, so it has to be
//     tracked (by index) and spilled if another flag comes along before its use

// calling convention:
// - scratch registers are caller saved (duh)
//   - opportunity for optimization here; after all functions are compiled, can
//     know whether or not a function actually clobbers a given register
// - parameters are passed on the stack
//   - this could probably be optimized more; note that stack pointer could be
//     decremented instead of pushing to the stack, because stuff past sp is
//     dead values
// - results are passed in caller saved registers, or on the stack if there are
//   too many

// block handling:
// - things that can be clobbered in a block:
//   - flags
//     - flags will always be clobbered in a block (otherwise it's a block
//     without conditions, which is stupid) so always spill them
//   - scratch registers
//     - put a nop at the start for each scratch reg thing in scope
//     - todo: would it be better to just put 16 nops for everything? prolly not
//   - by the end we know what's clobbered so that can be done anyways
// - things that can't be clobbered in a block:
//   - locals
//   - the stack

} // namespace arm64
} // namespace mitey