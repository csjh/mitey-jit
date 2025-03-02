#include "../../module.hpp"
#include "../../runtime.hpp"
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
    template <location loc> bool is() const { return this->loc == loc; }
};

#define SHARED_PARAMS std::byte *&code, WasmStack &stack

class Arm64 {
    template <typename T> static void put(std::byte *&code, const T &val) {
        std::memcpy(code, &val, sizeof(T));
        code += sizeof(T);
    }

    template <runtime::TrapKind kind> static void trap(std::byte *&code) {
        // put address of runtime::trap in x1
        // put kind in x0
        // br x1
    }

    using inst = uint32_t;

    static constexpr inst noop = 0xd503201f;

    static void orr(std::byte *&code, bool sf, shifttype shift, ireg rm,
                    uint8_t shift_imm, ireg rn, ireg rd) {
        put(code, 0b00101010000000000000000000000000 |
                      (static_cast<uint32_t>(sf) << 31) |
                      (static_cast<uint32_t>(shift) << 22) |
                      (static_cast<uint32_t>(rm) << 16) |
                      (static_cast<uint32_t>(shift_imm) << 10) |
                      (static_cast<uint32_t>(rn) << 5) |
                      (static_cast<uint32_t>(rd) << 0));
    }

    static void csinc(std::byte *&code, bool sf, ireg rm, cond c, ireg rn,
                      ireg rd) {
        put(code, 0b00011010100000000000010000000000 |
                      (static_cast<uint32_t>(sf) << 31) |
                      (static_cast<uint32_t>(rm) << 16) |
                      (static_cast<uint32_t>(c) << 12) |
                      (static_cast<uint32_t>(rn) << 5) |
                      (static_cast<uint32_t>(rd) << 0));
    }

    static void cset(std::byte *&code, bool sf, cond c, ireg rd) {
        csinc(code, sf, ireg::xzr, c, ireg::xzr, rd);
    }

    static void mov(std::byte *&code, ireg dst, ireg src) {
        orr(code, true, shifttype::lsl, src, 0, ireg::xzr, dst);
    }
    static void mov(std::byte *&code, freg dst, ireg src) {
        put(code, 0b10011110011001110000000000000000 |
                      (static_cast<uint32_t>(src) << 5) |
                      (static_cast<uint32_t>(dst) << 0));
    }
    static void mov(std::byte *&code, ireg dst, freg src) {
        put(code, 0b10011110011001100000000000000000 |
                      (static_cast<uint32_t>(src) << 5) |
                      (static_cast<uint32_t>(dst) << 0));
    }
    static void mov(std::byte *&code, freg dst, freg src) {
        put(code, 0b10011110011001110000000000000000 |
                      (static_cast<uint32_t>(src) << 5) |
                      (static_cast<uint32_t>(dst) << 0));
    }

    static void mov(std::byte *&code, bool sf, bool notneg, bool keep,
                    uint8_t hw, uint16_t imm, ireg rd) {
        put(code, 0b00010010100000000000000000000000 |
                      (static_cast<uint32_t>(sf) << 31) |
                      (static_cast<uint32_t>(notneg) << 30) |
                      (static_cast<uint32_t>(keep) << 29) |
                      (static_cast<uint32_t>(hw) << 21) |
                      (static_cast<uint32_t>(imm) << 5) |
                      (static_cast<uint8_t>(rd) << 0));
    }

    static void mov(std::byte *&code, ireg dst, uint64_t imm) {
        // todo: should this have a fast path for 0?
        // i think that should be handled elsewhere
        bool keep = false;
        for (size_t i = 0; i < sizeof(uint32_t) && imm; i++) {
            auto literal = imm & 0xffff;
            imm >>= 16;
            mov(code, true, true, keep, literal, i, dst);
            keep = true;
        }
        return;
    }

    // todo: secure these to allow for arbitrary offsets (slow case them)
    static void str_offset(std::byte *&code, uint16_t offset, ireg rn,
                           ireg rt) {
        put(code, 0b11111001000000000000000000000000 |
                      (static_cast<uint32_t>(offset) << 10) |
                      (static_cast<uint32_t>(rn) << 5) |
                      (static_cast<uint32_t>(rt) << 0));
    }
    static void str_offset(std::byte *&code, uint16_t offset, ireg rn,
                           freg rt) {
        put(code, 0b11111101000000000000000000000000 |
                      (static_cast<uint32_t>(offset) << 10) |
                      (static_cast<uint32_t>(rn) << 5) |
                      (static_cast<uint32_t>(rt) << 0));
    }

    static void ldr_offset(std::byte *&code, uint16_t offset, ireg rn,
                           ireg rt) {
        put(code, 0b11111001010000000000000000000000 |
                      (static_cast<uint32_t>(offset) << 10) |
                      (static_cast<uint32_t>(rn) << 5) |
                      (static_cast<uint32_t>(rt) << 0));
    }
    static void ldr_offset(std::byte *&code, uint16_t offset, ireg rn,
                           freg rt) {
        put(code, 0b11111101010000000000000000000000 |
                      (static_cast<uint32_t>(offset) << 10) |
                      (static_cast<uint32_t>(rn) << 5) |
                      (static_cast<uint32_t>(rt) << 0));
    }

    template <typename RegType, size_t First, size_t Last> class reg_manager {
      public:
        struct metadata {
            // when a register is inactive, this is a null pointer
            // when a register is given a value, this is set to a noop
            // when a register is stolen, this is set to a spill
            std::byte *spilladdr = dummy;
            uint32_t stack_offset = 0;
            std::byte dummy[4];
        };

      private:
        reg_lru regs;
        metadata data[Last - First];

        void spill(uint8_t reg) {
            auto [addr, offset] = data[reg - First];
            str_offset(addr, offset, stackreg, reg);
        }

      public:
        std::pair<RegType, metadata *> steal(std::byte *&code);
        void surrender(RegType reg);
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

    void init(value *locals, size_t n) {
        this->locals = std::span<value>(locals, n);
    }

    void clobber_flags(std::byte *&code) {
        if (!flag.val)
            return;

        // step 1. claim a register, spilling if necessary
        auto [spill, metadata] = intregs.steal(code);
        // step 2. spill into claimed register
        cset(code, true, flag.val->as<cond>(), spill);
        // step 3. set register metadata (for spilling)
        *metadata = decltype(intregs)::metadata(code, flag.stack_offset);

        *flag.val = value::reg(spill);
        flag.val = nullptr;
    }

    void push(value v) {
        *values++ = v;
        // consts don't occupy stack space
        if (!v.is<value::location::imm>()) {
            stack_size += sizeof(runtime::WasmValue);
        }
    }

    void pop();

  public:
    // todo: figure out what values for these
    static constexpr size_t function_overhead = 100 * sizeof(uint32_t);
    static constexpr size_t max_instruction = 100 * sizeof(uint32_t);

    void start_function(SHARED_PARAMS, FunctionShell &fn) {
        // stp     x29, x30, [sp, #-0x10]!
        // mov     x29, sp
        put(code, std::array<uint32_t, 2>{0xa9bf7bfd, 0x910003fd});

        auto locals = new value[fn.locals.size()];

        auto ireg_alloc = icallee_saved.begin();
        auto freg_alloc = fcallee_saved.begin();

        for (auto i = 0; i < fn.locals.size(); i++) {
            auto local = fn.locals[i];
            auto adjusted = i - fn.type.params.size();
            auto offset = adjusted < 0
                              ? 0x10 - adjusted * sizeof(runtime::WasmValue)
                              : adjusted * sizeof(runtime::WasmValue);

            if ((local == valtype::i32 || local == valtype::i64 ||
                 local == valtype::funcref || local == valtype::externref) &&
                ireg_alloc != icallee_saved.end()) {
                // save current value
                mov(code, ireg::x3, *ireg_alloc);
                ldr_offset(code, offset, stackreg, *ireg_alloc);
                str_offset(code, offset, stackreg, ireg::x3);

                locals[i] = value::reg(*ireg_alloc++);
            } else if ((local == valtype::f32 || local == valtype::f64) &&
                       freg_alloc != fcallee_saved.end()) {
                // save current value
                mov(code, freg::d0, *freg_alloc);
                ldr_offset(code, offset, stackreg, *freg_alloc);
                str_offset(code, offset, stackreg, freg::d0);

                locals[i] = value::reg(*freg_alloc++);
            } else {
                locals[i] = value::stack(offset);
            }
        }

        // ...my stack | non-parameter locals | link/stack | parameters
        //                               sp --^
        // at call site:
        // decrement sp past parameters
        // call
        // increment sp back to after link/stack

        init(locals, fn.locals.size());
    }
    void exit_function(SHARED_PARAMS, FunctionShell &fn) {
        // ldp     x29, x30, [sp], #0x10
        // ret
        put(code, std::array<uint32_t, 2>{0xa8c17bfd, 0xd65f03c0});

        delete[] locals.data();
    }

    void unreachable(SHARED_PARAMS) {
        trap<runtime::TrapKind::unreachable>(code);
    }
    void nop(SHARED_PARAMS) { put(code, noop); }
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
    void localget(SHARED_PARAMS, FunctionShell &fn, uint32_t local_idx) {
        push(locals[local_idx]);
    }
    void localset(SHARED_PARAMS, FunctionShell &fn, uint32_t local_idx);
    void localtee(SHARED_PARAMS, FunctionShell &fn, uint32_t local_idx);
    void tableget(SHARED_PARAMS, uint64_t misc_offset);
    void tableset(SHARED_PARAMS, uint64_t misc_offset);
    void globalget(SHARED_PARAMS, uint64_t misc_offset);
    void globalset(SHARED_PARAMS, uint64_t misc_offset);
    void memorysize(SHARED_PARAMS);
    void memorygrow(SHARED_PARAMS);
    void i32const(SHARED_PARAMS, uint32_t cons) { push(value::imm(cons)); }
    void i64const(SHARED_PARAMS, uint64_t cons) {
        if (cons <= std::numeric_limits<uint32_t>::max()) {
            mov(code, ireg::x0, cons);
        } else {
            push(value::imm(cons));
        }
    }
    void f32const(SHARED_PARAMS, float cons) {
        mov(code, ireg::x0, std::bit_cast<uint32_t>(cons));
        mov(code, freg::d0, ireg::x0);
    }
    void f64const(SHARED_PARAMS, double cons) {
        mov(code, ireg::x0, std::bit_cast<uint64_t>(cons));
        mov(code, freg::d0, ireg::x0);
    }
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