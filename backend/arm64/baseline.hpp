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
    enum class location { reg, stack, imm, flags };

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
        return value(location::flags, static_cast<uint32_t>(c));
    }

    template <typename T> T as() const { return static_cast<T>(val); }
    location where() const { return loc; }
    template <location loc> bool is() const { return where() == loc; }
};

#define SHARED_PARAMS                                                          \
    [[maybe_unused]] std::byte *&code, [[maybe_unused]] WasmStack &stack

class Arm64 {
  public:
    template <auto registers> class temp_reg_manager {
        using RegType = decltype(registers)::value_type;
        static constexpr auto First = (size_t)registers.front();
        static constexpr auto Last = (size_t)registers.back();
        static constexpr auto N = registers.size();
        static_assert(N == Last - First + 1, "registers must be contiguous");

      public:
        struct metadata {
            // when a register is inactive, this is a null pointer
            // when a register is given a value, this is set to a noop
            // when a register is stolen, this is set to a spill
            std::byte *spilladdr = nullptr;
            // this could be an index into the values pointer but
            // that's an optimization attempt for another day
            value *value_offset = nullptr;
            uint32_t stack_offset = 0;
        };

      private:
        reg_lru<N> regs;
        metadata data[N];

        uint8_t to_index(RegType reg);
        RegType from_index(uint8_t idx);

      public:
        void reset_temporaries();
        // takes the least recently used non-claimed register
        // spills the register if necessary
        // result should only used after all temporaries are consumed
        // i.e. <result> = <temp1> + <temp2> (good)
        // <result> = <temp1> + <temp2>
        // <result> = <result> + <temp1> (bad)
        // because <result> might alias temp1
        RegType result();
        // adds the spill metadata for a given (result) register
        void claim(RegType, metadata);
        // takes the least recently used register
        // spills the register if necessary
        RegType temporary();
        // dumps a register, throws away the metadata
        void surrender(RegType reg);

        void clobber_all();
        bool check_spill(RegType reg, std::byte *code);
    };

    using temp_int_manager = temp_reg_manager<icaller_saved>;
    using temp_float_manager = temp_reg_manager<fcaller_saved>;

    template <typename RegType, size_t N> class lasting_reg_manager {
      public:
        struct metadata {
            std::byte *spilladdr = nullptr;
            value *value_offset = nullptr;
            uint32_t stack_offset = 0;
        };

      private:
        metadata data[N];
        size_t count = 0;

        void spill(RegType, size_t);

      public:
        void claim(RegType, metadata);
        void surrender(value *);
        void purge(RegType);
        bool check_spill(RegType reg, std::byte *code);
    };

    template <auto registers> class local_manager {
        using RegType = decltype(registers)::value_type;
        static constexpr auto First = (size_t)registers.front();
        static constexpr auto Last = (size_t)registers.back();
        static constexpr auto N = registers.size();
        static_assert(N == Last - First + 1, "registers must be contiguous");

        using sub_manager = lasting_reg_manager<RegType, 4>;

        sub_manager locals[N];

        sub_manager &get_manager_of(RegType reg) {
            return locals[(size_t)reg - First];
        }

      public:
        void claim(RegType, sub_manager::metadata);
        void surrender(RegType, value *);
        void purge(RegType);
        bool check_spill(RegType reg, std::byte *code);
    };

  private:
    // caller saved registers
    temp_int_manager intregs;
    temp_float_manager floatregs;

    // callee saved registers
    std::span<value> locals;
    local_manager<icallee_saved> intlocals;
    local_manager<fcallee_saved> floatlocals;

    struct flags {
        // offset to spill into
        uint32_t stack_offset = 0;
        // pointer into values pointing to flag value (or nil)
        value *val = nullptr;
    };
    flags flag;

    uint32_t stack_size = 0;
    std::unique_ptr<value[]> values_start =
        std::make_unique_for_overwrite<value[]>(65536);
    value *values = values_start.get();

    void clobber_flags(std::byte *&code);
    void clobber_registers();

    void push(value v);

    class iwant {
        struct thresholdless {
            static constexpr uint64_t threshold = 0;
        };

      public:
        struct none : thresholdless {};
        struct ireg : thresholdless {};
        struct freg : thresholdless {};
        template <uint64_t t = 1ull << 32> struct literal {
            static constexpr uint64_t threshold = t;
        };
        template <typename T> struct bitmask : thresholdless {
            using type = T;
        };
        struct flags : thresholdless {};
    };

    template <typename To> value adapt_value(std::byte *&code, value *v);
    ireg adapt_value_into(std::byte *&code, value *v, std::optional<ireg> &reg,
                          bool soft = false);
    freg adapt_value_into(std::byte *&code, value *v, std::optional<freg> &reg,
                          bool soft = false);

    void stackify(std::byte *&code, valtype_vector &values);
    bool move_results(std::byte *&code, valtype_vector &copied_values,
                      uint32_t copy_to, bool discard_copied);
    void discard(std::byte *&code, WasmStack &stack, uint32_t skip,
                 uint32_t discard_to);

    void pad_spill(std::byte *&code, uint32_t stack_size);

    template <typename FloatType>
    void validate_trunc(std::byte *&code, freg v, FloatType lower,
                        FloatType upper);

    // eventually i'll have to support multivalue here
    // honestly i don't think it'll even be that difficult (single digit lines)
    // 🤞 but there's no multivalue instructions (yet)
    template <typename Params, typename Result>
    std::array<value,
               std::tuple_size_v<Params> + !std::is_same_v<Result, iwant::none>>
    allocate_registers(std::byte *&code);

    template <typename... Args>
    void finalize(std::byte *&code, Args... results);

    void amend_br(std::byte *br, std::byte *target);
    void amend_br_if(std::byte *br, std::byte *target);

    void exit_function(SHARED_PARAMS, ControlFlow &flow);

    template <memtype mtype, resexttype etype, bool is_float>
    void abstract_memop(SHARED_PARAMS, uint64_t offset);

    template <runtime::Signature func, size_t NP, size_t NR>
    void runtime_call(std::byte *&code, std::array<valtype, NP> params,
                      std::array<valtype, NR> results,
                      std::optional<uint64_t> temp1 = std::nullopt,
                      std::optional<uint64_t> temp2 = std::nullopt);

  public:
    // todo: figure out what values for these
    static constexpr size_t function_overhead = 100 * sizeof(uint32_t);
    static constexpr size_t max_instruction = 100 * sizeof(uint32_t);

    void start_function(SHARED_PARAMS, FunctionShell &fn);

    void unreachable(SHARED_PARAMS);
    void nop(SHARED_PARAMS);
    void block(SHARED_PARAMS, WasmSignature &sig);
    void loop(SHARED_PARAMS, WasmSignature &sig);
    std::byte *if_(SHARED_PARAMS, WasmSignature &sig);
    void else_(SHARED_PARAMS, std::span<ControlFlow> control_stack);
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
    void drop(SHARED_PARAMS, valtype type);
    void select(SHARED_PARAMS, valtype type);
    void select_t(SHARED_PARAMS, valtype type);
    void localget(SHARED_PARAMS, FunctionShell &fn, uint32_t local_idx);
    void localset(SHARED_PARAMS, FunctionShell &fn, uint32_t local_idx);
    void localtee(SHARED_PARAMS, FunctionShell &fn, uint32_t local_idx);
    void tableget(SHARED_PARAMS, uint64_t misc_offset);
    void tableset(SHARED_PARAMS, uint64_t misc_offset);
    void globalget(SHARED_PARAMS, uint64_t misc_offset, valtype type);
    void globalset(SHARED_PARAMS, uint64_t misc_offset, valtype type);
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

constexpr auto x = sizeof(Arm64);

} // namespace arm64
} // namespace mitey