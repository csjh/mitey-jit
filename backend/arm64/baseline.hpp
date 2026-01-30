#pragma once

#include "../../module.hpp"
#include "./enums.hpp"
#include "./reg_lru.hpp"
#include <cstddef>
#include <cstdint>
#include <span>

namespace mitey {
namespace arm64 {

using inst = uint32_t;

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

    bool operator==(const value &other) const {
        return loc == other.loc && val == other.val;
    }
};

template <typename Src, typename Dest = Src> struct edge {
    Src src;
    Dest dest;
};

#define SHARED_PARAMS                                                          \
    [[maybe_unused]] std::byte *&code, [[maybe_unused]] WasmStack &stack

class Arm64 {
    struct metadata {
        std::byte *source_location = nullptr;
        value *value_offset = nullptr;
        uint32_t stack_offset = 0;
    };

  public:
    template <typename RegType, uint32_t N> class reg_info {
        std::byte *spilladdr = nullptr;
        std::byte *source_location = nullptr;
        uint32_t stack_offset = 0;
        uint32_t count = 0;
        value *values[N];
        uint8_t _padding[8];

        void spill(std::byte *&, RegType, uint32_t i);

      public:
        void use(std::byte *&, RegType, metadata);
        bool surrender(value *);
        [[nodiscard]] int32_t purge(std::byte *&, RegType);
        bool can_overwrite(std::byte *code);
        bool is_free();
        void set_spill(std::byte *&);
    };

    template <auto registers> class reg_manager {
        using RegType = typename decltype(registers)::value_type;
        static constexpr bool allocate =
            std::is_same_v<decltype(registers),
                           std::remove_cv_t<decltype(icaller_saved)>> ||
            std::is_same_v<decltype(registers),
                           std::remove_cv_t<decltype(fcaller_saved)>>;
        static constexpr auto First = (size_t)registers.front();
        static constexpr auto Last = (size_t)registers.back();
        static constexpr auto N = registers.size();
        static_assert(N == Last - First + 1, "registers must be contiguous");
        using sub_manager = reg_info<RegType, 4>;

        class local_manager {
            struct plane {
                std::byte *dumpaddr;
                uint32_t local_idx;
                bool active = false;
            };

            std::array<plane, allocate ? N : 0> inflight_locals;

            void commit(RegType reg);

          public:
            [[nodiscard]] int32_t activate(std::byte *&code, value *locals,
                                           uint32_t local_idx, RegType reg,
                                           bool set);
            void deactivate(value *locals, RegType reg);
            [[nodiscard]] int32_t deactivate_all(value *locals);
            void commit_all();
            bool is_active(RegType reg);
        };

        local_manager locals;
        sub_manager regs[N];
        int32_t interest = 0;
        int32_t activity = 0;

        sub_manager &get_manager_of(RegType reg) {
            return regs[static_cast<uint8_t>(reg) - First];
        }

        reg_lru<allocate ? N : 0> reg_positions;

        static uint8_t to_index(RegType reg) {
            return static_cast<uint8_t>(reg) - First;
        }
        static RegType from_index(uint8_t idx) {
            return static_cast<RegType>(idx + First);
        }

      public:
        RegType result(std::byte *&, value *locals);
        RegType temporary(std::byte *&, value *locals);
        void untemporary(RegType reg);
        void reset_temporaries();

        void activate(std::byte *&code, value *locals, uint32_t local_idx,
                      RegType reg, bool set);
        void deactivate_all(value *locals);
        void commit_all();

        void use(std::byte *&, RegType, metadata);
        void surrender(RegType, value *);
        void purge(std::byte *&, value *locals, RegType);
        void spill_all(std::byte *&, value *locals);
        void set_spills(std::byte *&);

        bool can_overwrite(RegType reg, std::byte *code);
        bool is_free(RegType reg);
    };

    using temp_int_manager = reg_manager<icaller_saved>;
    using temp_float_manager = reg_manager<fcaller_saved>;

    template <typename RegType> struct temporary {
        static_assert(std::is_same_v<RegType, ireg> ||
                          std::is_same_v<RegType, freg>,
                      "temporary can only be used with ireg or freg");

        using manager_type = std::conditional_t<std::is_same_v<RegType, ireg>,
                                                Arm64::temp_int_manager,
                                                Arm64::temp_float_manager>;

        temporary(Arm64 *that, std::byte *&code)
            : that(that), reg(that->regs_of<RegType>().temporary(
                              code, that->values_start)) {}

        temporary(RegType existing) {
            that = nullptr;
            reg = existing;
        }

        temporary(temporary<RegType> &existing) = delete;
        temporary(temporary<RegType> &&existing) {
            that = existing.that;
            reg = existing.reg;
            existing.that = nullptr;
        }

        ~temporary() {
            if (that)
                that->regs_of<RegType>().untemporary(reg);
        }

        void operator=(temporary<RegType> &existing) = delete;
        void operator=(temporary<RegType> &&existing) = delete;
        operator RegType() { return reg; }
        RegType get() { return reg; }

      private:
        Arm64 *that = nullptr;
        RegType reg;
    };

  private:
    template <typename RegType> bool is_free(RegType);
    template <typename RegType> void use_no_overwrite(std::byte *&, RegType);
    template <typename RegType> void use(std::byte *&, RegType);
    template <typename RegType> void surrender(RegType, value *);
    template <typename RegType> void purge(std::byte *&, RegType);
    template <typename RegType>
    bool can_overwrite(RegType reg, std::byte *code);

    void polymorph(valtype ty, auto &&func) {
        if (is_float(ty)) [[unlikely]] {
            func(freg{});
        } else {
            func(ireg{});
        }
    }

    static value _values_start[65536];
    value *values_start = _values_start;
    value *values = _values_start;
    inline value *locals() { return values_start; }
    uint32_t stack_size = 0;

    // caller saved registers
    temp_int_manager intregs;
    temp_float_manager floatregs;

    template <typename RegType> auto &regs_of() {
        if constexpr (std::is_same_v<RegType, ireg>) {
            return intregs;
        } else if constexpr (std::is_same_v<RegType, freg>) {
            return floatregs;
        } else {
            static_assert(!std::is_same_v<RegType, RegType>,
                          "Invalid register type");
        }
    }

    // callee saved registers
    reg_manager<icallee_saved> intlocals;
    reg_manager<fcallee_saved> floatlocals;

    template <typename RegType> auto &locals_of() {
        if constexpr (std::is_same_v<RegType, ireg>) {
            return intlocals;
        } else if constexpr (std::is_same_v<RegType, freg>) {
            return floatlocals;
        } else {
            static_assert(!std::is_same_v<RegType, RegType>,
                          "Invalid register type");
        }
    }

    struct flags {
        // offset to spill into
        uint32_t stack_offset = 0;
        // pointer into values pointing to flag value (or nil)
        value *val = nullptr;
    };
    flags flag;

    void spill_flags_to_register(std::byte *&code);
    void spill_flags_to_stack(std::byte *&code);
    void spill_registers(std::byte *&);

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

    template <typename To>
    void force_value_into(std::byte *&code, value *v, To reg,
                          bool soft = false);

    template <typename To>
    temporary<To> adapt_value_into(std::byte *&code, value *v,
                                   bool soft = false);

    void move_single(std::byte *&code, valtype ty, value *expected,
                     uint32_t dest, bool discard_copied, bool constrained);
    bool move_results(std::byte *&code, std::span<valtype> copied_values,
                      uint32_t copy_to, bool discard_copied);
    bool move_block_results(std::byte *&code, std::span<valtype> copied_values,
                            uint32_t copy_to, bool discard_copied);
    void push_block_results(std::byte *&code, std::span<valtype> values);
    void discard(std::byte *&code, WasmStack &stack, uint32_t skip,
                 uint32_t discard_to);

    template <auto t, typename Ty>
    void unop(SHARED_PARAMS, void (*op)(std::byte *&, decltype(t), Ty, Ty));

    template <auto t, typename Ty>
    void binop(SHARED_PARAMS,
               void (*op)(std::byte *&, decltype(t), Ty, Ty, Ty));

    template <bool is_64, typename ImmTy, typename Ty>
    void binop_imm(SHARED_PARAMS, void (*op)(std::byte *&, bool, Ty, Ty, Ty),
                   void (*imm_op)(std::byte *&, bool, ImmTy, Ty, Ty));

    template <typename FloatType>
    void validate_trunc(std::byte *&code, freg v, FloatType lower,
                        FloatType upper);

    template <auto t1, auto t2, auto lower = 0, auto upper = 0, typename InType,
              typename OutType>
    void conversion(SHARED_PARAMS, void (*op)(std::byte *&, decltype(t1),
                                              decltype(t2), InType, OutType));

    template <cond c, bool is_64, bool is_float, bool is_eqz>
    void comparison(SHARED_PARAMS);

    // eventually i'll have to support multivalue here
    // honestly i don't think it'll even be that difficult (single digit lines)
    // 🤞 but there's no multivalue instructions (yet)
    template <typename Params, typename Result>
    std::array<value,
               std::tuple_size_v<Params> + !std::is_same_v<Result, iwant::none>>
    allocate_registers(std::byte *&code);

    template <typename... Args>
    void finalize(std::byte *&code, Args... results);

    static void amend_br(std::byte *br, std::byte *target);
    static void amend_br_if(std::byte *br, std::byte *target);

    void exit_function(SHARED_PARAMS, ControlFlow &flow);

    template <memtype mtype, resexttype etype, bool is_float>
    void abstract_memop(SHARED_PARAMS, uint64_t offset);

    template <runtime::Signature func, size_t NP, size_t NR>
    void runtime_call(std::byte *&code, std::array<valtype, NP> params,
                      std::array<valtype, NR> results,
                      std::optional<uint64_t> temp1 = std::nullopt,
                      std::optional<uint64_t> temp2 = std::nullopt);

    template <typename T>
    void negotiate_registers(std::byte *&, std::span<edge<T>>);
    template <typename T>
    void __attribute__((noinline, preserve_most))
    negotiate_registers_slowpath(std::byte *&, std::span<edge<T>>);

    void conventionalize(std::byte *&, WasmStack &, std::span<valtype>);

  public:
    // todo: figure out what values for these
    // todo: account for trampolines in function_overhead
    static constexpr size_t function_overhead = 100 * sizeof(uint32_t);
    static constexpr size_t max_instruction = 100 * sizeof(uint32_t);

    static std::byte *generate_trampoline(std::byte *&, uint32_t,
                                          FunctionShell &);

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

} // namespace arm64
} // namespace mitey