#include "../../module.hpp"
#include <array>
#include <cstddef>
#include <cstdint>
#include <span>

namespace mitey {
namespace arm64 {

// clang-format off
enum class ireg {
    x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15,
    x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, /* x29, x30, x31, */
};

enum class freg {
    d0, d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, d14, d15,
    d16, d17, d18, d19, d20, d21, d22, d23, d24, d25, d26, d27, d28, d29, d30, d31,
};

constexpr auto callee_saved = std::to_array({
    ireg::x19, ireg::x20, ireg::x21, ireg::x22, ireg::x23,
    ireg::x24, ireg::x25, ireg::x26, ireg::x27, ireg::x28});

constexpr auto caller_saved = std::to_array({
    ireg::x0, ireg::x1, ireg::x2, ireg::x3, ireg::x4, ireg::x5, ireg::x6, ireg::x7,
    ireg::x8, ireg::x9, ireg::x10, ireg::x11, ireg::x12, ireg::x13, ireg::x14, ireg::x15,
    ireg::x16, ireg::x17, ireg::x18});
// clang-format on

// todo: maybe put in callee saved? can bench
constexpr auto memreg = ireg::x0;
constexpr auto miscreg = ireg::x1;

#define SHARED_PARAMS std::byte *&code, WasmStack &stack, extra &_extra

class Arm64 {
  public:
    using extra = int;

    // todo: figure out what values for these
    static constexpr size_t function_overhead = 100 * sizeof(uint32_t);
    static constexpr size_t max_instruction = 100 * sizeof(uint32_t);

    static void put_call_address(std::byte *&code, std::byte *func);

    static void start_function(SHARED_PARAMS, FunctionShell &fn);
    static void exit_function(SHARED_PARAMS, FunctionShell &fn);

    static void unreachable(SHARED_PARAMS);
    static void nop(SHARED_PARAMS);
    static void block(SHARED_PARAMS, WasmSignature &sig);
    static void loop(SHARED_PARAMS, WasmSignature &sig);
    static std::byte *if_(SHARED_PARAMS, WasmSignature &sig);
    static std::byte *else_(SHARED_PARAMS, WasmSignature &sig,
                            std::byte *if_location);
    static void end(SHARED_PARAMS, ControlFlow &flow);
    static void br(SHARED_PARAMS, std::span<ControlFlow> control_stack,
                   uint32_t depth);
    static void br_if(SHARED_PARAMS, std::span<ControlFlow> control_stack,
                      uint32_t depth);
    static void br_table(SHARED_PARAMS, std::span<ControlFlow> control_stack,
                         std::span<uint32_t> targets);
    static void return_(SHARED_PARAMS, std::span<ControlFlow> control_stack);
    static void call_extern(SHARED_PARAMS, FunctionShell &fn,
                            uint32_t func_offset);
    static std::byte *call(SHARED_PARAMS, FunctionShell &fn);
    static void call_indirect(SHARED_PARAMS, uint32_t table_offset,
                              WasmSignature &type);
    static void drop(SHARED_PARAMS);
    static void select(SHARED_PARAMS);
    static void select_t(SHARED_PARAMS);
    static void localget(SHARED_PARAMS, FunctionShell &fn, uint32_t local_idx);
    static void localset(SHARED_PARAMS, FunctionShell &fn, uint32_t local_idx);
    static void localtee(SHARED_PARAMS, FunctionShell &fn, uint32_t local_idx);
    static void tableget(SHARED_PARAMS, uint64_t misc_offset);
    static void tableset(SHARED_PARAMS, uint64_t misc_offset);
    static void globalget(SHARED_PARAMS, uint64_t misc_offset);
    static void globalset(SHARED_PARAMS, uint64_t misc_offset);
    static void memorysize(SHARED_PARAMS);
    static void memorygrow(SHARED_PARAMS);
    static void i32const(SHARED_PARAMS, uint32_t cons);
    static void i64const(SHARED_PARAMS, uint64_t cons);
    static void f32const(SHARED_PARAMS, float cons);
    static void f64const(SHARED_PARAMS, double cons);
    static void i32load(SHARED_PARAMS, uint64_t offset, uint64_t align);
    static void i64load(SHARED_PARAMS, uint64_t offset, uint64_t align);
    static void f32load(SHARED_PARAMS, uint64_t offset, uint64_t align);
    static void f64load(SHARED_PARAMS, uint64_t offset, uint64_t align);
    static void i32load8_s(SHARED_PARAMS, uint64_t offset, uint64_t align);
    static void i32load8_u(SHARED_PARAMS, uint64_t offset, uint64_t align);
    static void i32load16_s(SHARED_PARAMS, uint64_t offset, uint64_t align);
    static void i32load16_u(SHARED_PARAMS, uint64_t offset, uint64_t align);
    static void i64load8_s(SHARED_PARAMS, uint64_t offset, uint64_t align);
    static void i64load8_u(SHARED_PARAMS, uint64_t offset, uint64_t align);
    static void i64load16_s(SHARED_PARAMS, uint64_t offset, uint64_t align);
    static void i64load16_u(SHARED_PARAMS, uint64_t offset, uint64_t align);
    static void i64load32_s(SHARED_PARAMS, uint64_t offset, uint64_t align);
    static void i64load32_u(SHARED_PARAMS, uint64_t offset, uint64_t align);
    static void i32store(SHARED_PARAMS, uint64_t offset, uint64_t align);
    static void i64store(SHARED_PARAMS, uint64_t offset, uint64_t align);
    static void f32store(SHARED_PARAMS, uint64_t offset, uint64_t align);
    static void f64store(SHARED_PARAMS, uint64_t offset, uint64_t align);
    static void i32store8(SHARED_PARAMS, uint64_t offset, uint64_t align);
    static void i32store16(SHARED_PARAMS, uint64_t offset, uint64_t align);
    static void i64store8(SHARED_PARAMS, uint64_t offset, uint64_t align);
    static void i64store16(SHARED_PARAMS, uint64_t offset, uint64_t align);
    static void i64store32(SHARED_PARAMS, uint64_t offset, uint64_t align);
    static void i32eqz(SHARED_PARAMS);
    static void i64eqz(SHARED_PARAMS);
    static void i32eq(SHARED_PARAMS);
    static void i64eq(SHARED_PARAMS);
    static void i32ne(SHARED_PARAMS);
    static void i64ne(SHARED_PARAMS);
    static void i32lt_s(SHARED_PARAMS);
    static void i64lt_s(SHARED_PARAMS);
    static void i32lt_u(SHARED_PARAMS);
    static void i64lt_u(SHARED_PARAMS);
    static void i32gt_s(SHARED_PARAMS);
    static void i64gt_s(SHARED_PARAMS);
    static void i32gt_u(SHARED_PARAMS);
    static void i64gt_u(SHARED_PARAMS);
    static void i32le_s(SHARED_PARAMS);
    static void i64le_s(SHARED_PARAMS);
    static void i32le_u(SHARED_PARAMS);
    static void i64le_u(SHARED_PARAMS);
    static void i32ge_s(SHARED_PARAMS);
    static void i64ge_s(SHARED_PARAMS);
    static void i32ge_u(SHARED_PARAMS);
    static void i64ge_u(SHARED_PARAMS);
    static void f32eq(SHARED_PARAMS);
    static void f64eq(SHARED_PARAMS);
    static void f32ne(SHARED_PARAMS);
    static void f64ne(SHARED_PARAMS);
    static void f32lt(SHARED_PARAMS);
    static void f64lt(SHARED_PARAMS);
    static void f32gt(SHARED_PARAMS);
    static void f64gt(SHARED_PARAMS);
    static void f32le(SHARED_PARAMS);
    static void f64le(SHARED_PARAMS);
    static void f32ge(SHARED_PARAMS);
    static void f64ge(SHARED_PARAMS);
    static void i32clz(SHARED_PARAMS);
    static void i64clz(SHARED_PARAMS);
    static void i32ctz(SHARED_PARAMS);
    static void i64ctz(SHARED_PARAMS);
    static void i32popcnt(SHARED_PARAMS);
    static void i64popcnt(SHARED_PARAMS);
    static void i32add(SHARED_PARAMS);
    static void i64add(SHARED_PARAMS);
    static void i32sub(SHARED_PARAMS);
    static void i64sub(SHARED_PARAMS);
    static void i32mul(SHARED_PARAMS);
    static void i64mul(SHARED_PARAMS);
    static void i32div_s(SHARED_PARAMS);
    static void i64div_s(SHARED_PARAMS);
    static void i32div_u(SHARED_PARAMS);
    static void i64div_u(SHARED_PARAMS);
    static void i32rem_s(SHARED_PARAMS);
    static void i64rem_s(SHARED_PARAMS);
    static void i32rem_u(SHARED_PARAMS);
    static void i64rem_u(SHARED_PARAMS);
    static void i32and(SHARED_PARAMS);
    static void i64and(SHARED_PARAMS);
    static void i32or(SHARED_PARAMS);
    static void i64or(SHARED_PARAMS);
    static void i32xor(SHARED_PARAMS);
    static void i64xor(SHARED_PARAMS);
    static void i32shl(SHARED_PARAMS);
    static void i64shl(SHARED_PARAMS);
    static void i32shr_s(SHARED_PARAMS);
    static void i64shr_s(SHARED_PARAMS);
    static void i32shr_u(SHARED_PARAMS);
    static void i64shr_u(SHARED_PARAMS);
    static void i32rotl(SHARED_PARAMS);
    static void i64rotl(SHARED_PARAMS);
    static void i32rotr(SHARED_PARAMS);
    static void i64rotr(SHARED_PARAMS);
    static void f32abs(SHARED_PARAMS);
    static void f64abs(SHARED_PARAMS);
    static void f32neg(SHARED_PARAMS);
    static void f64neg(SHARED_PARAMS);
    static void f32ceil(SHARED_PARAMS);
    static void f64ceil(SHARED_PARAMS);
    static void f32floor(SHARED_PARAMS);
    static void f64floor(SHARED_PARAMS);
    static void f32trunc(SHARED_PARAMS);
    static void f64trunc(SHARED_PARAMS);
    static void f32nearest(SHARED_PARAMS);
    static void f64nearest(SHARED_PARAMS);
    static void f32sqrt(SHARED_PARAMS);
    static void f64sqrt(SHARED_PARAMS);
    static void f32add(SHARED_PARAMS);
    static void f64add(SHARED_PARAMS);
    static void f32sub(SHARED_PARAMS);
    static void f64sub(SHARED_PARAMS);
    static void f32mul(SHARED_PARAMS);
    static void f64mul(SHARED_PARAMS);
    static void f32div(SHARED_PARAMS);
    static void f64div(SHARED_PARAMS);
    static void f32min(SHARED_PARAMS);
    static void f64min(SHARED_PARAMS);
    static void f32max(SHARED_PARAMS);
    static void f64max(SHARED_PARAMS);
    static void f32copysign(SHARED_PARAMS);
    static void f64copysign(SHARED_PARAMS);
    static void i32wrap_i64(SHARED_PARAMS);
    static void i64extend_i32_s(SHARED_PARAMS);
    static void i64extend_i32_u(SHARED_PARAMS);
    static void i32trunc_f32_s(SHARED_PARAMS);
    static void i64trunc_f32_s(SHARED_PARAMS);
    static void i32trunc_f32_u(SHARED_PARAMS);
    static void i64trunc_f32_u(SHARED_PARAMS);
    static void i32trunc_f64_s(SHARED_PARAMS);
    static void i64trunc_f64_s(SHARED_PARAMS);
    static void i32trunc_f64_u(SHARED_PARAMS);
    static void i64trunc_f64_u(SHARED_PARAMS);
    static void f32convert_i32_s(SHARED_PARAMS);
    static void f64convert_i32_s(SHARED_PARAMS);
    static void f32convert_i32_u(SHARED_PARAMS);
    static void f64convert_i32_u(SHARED_PARAMS);
    static void f32convert_i64_s(SHARED_PARAMS);
    static void f64convert_i64_s(SHARED_PARAMS);
    static void f32convert_i64_u(SHARED_PARAMS);
    static void f64convert_i64_u(SHARED_PARAMS);
    static void f32demote_f64(SHARED_PARAMS);
    static void f64promote_f32(SHARED_PARAMS);
    static void i32reinterpret_f32(SHARED_PARAMS);
    static void f32reinterpret_i32(SHARED_PARAMS);
    static void i64reinterpret_f64(SHARED_PARAMS);
    static void f64reinterpret_i64(SHARED_PARAMS);
    static void i32extend8_s(SHARED_PARAMS);
    static void i32extend16_s(SHARED_PARAMS);
    static void i64extend8_s(SHARED_PARAMS);
    static void i64extend16_s(SHARED_PARAMS);
    static void i64extend32_s(SHARED_PARAMS);
    static void ref_null(SHARED_PARAMS);
    static void ref_is_null(SHARED_PARAMS);
    static void ref_func(SHARED_PARAMS, uint64_t misc_offset);
    static void ref_eq(SHARED_PARAMS);
    static void i32_trunc_sat_f32_s(SHARED_PARAMS);
    static void i32_trunc_sat_f32_u(SHARED_PARAMS);
    static void i32_trunc_sat_f64_s(SHARED_PARAMS);
    static void i32_trunc_sat_f64_u(SHARED_PARAMS);
    static void i64_trunc_sat_f32_s(SHARED_PARAMS);
    static void i64_trunc_sat_f32_u(SHARED_PARAMS);
    static void i64_trunc_sat_f64_s(SHARED_PARAMS);
    static void i64_trunc_sat_f64_u(SHARED_PARAMS);
    static void memory_init(SHARED_PARAMS, uint64_t misc_offset);
    static void data_drop(SHARED_PARAMS, uint64_t misc_offset);
    static void memory_copy(SHARED_PARAMS);
    static void memory_fill(SHARED_PARAMS);
    static void table_init(SHARED_PARAMS, uint64_t seg_offset,
                           uint64_t table_offset);
    static void elem_drop(SHARED_PARAMS, uint64_t misc_offset);
    static void table_copy(SHARED_PARAMS, uint64_t dst_offset,
                           uint64_t src_offset);
    static void table_grow(SHARED_PARAMS, uint64_t misc_offset);
    static void table_size(SHARED_PARAMS, uint64_t misc_offset);
    static void table_fill(SHARED_PARAMS, uint64_t misc_offset);
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