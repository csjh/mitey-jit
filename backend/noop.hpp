#include "../module.hpp"

#define SHARED_PARAMS std::byte *&code, WasmStack &stack, extra &_extra

namespace mitey {

class Noop {
  public:
    using extra = int;

    static constexpr size_t function_overhead = 1;
    static constexpr size_t max_instruction = 1;

    static void put_call_address(std::byte *&code, std::byte *func) {}

    static void start_function(SHARED_PARAMS, FunctionShell &fn) {}
    static void exit_function(SHARED_PARAMS, FunctionShell &fn) {}

    static void unreachable(SHARED_PARAMS) {}
    static void nop(SHARED_PARAMS) {}
    static void block(SHARED_PARAMS, WasmSignature &sig) {}
    static void loop(SHARED_PARAMS, WasmSignature &sig) {}
    static std::byte *if_(SHARED_PARAMS, WasmSignature &sig) {return nullptr;}
    static std::byte *else_(SHARED_PARAMS, WasmSignature &sig,
                            std::byte *if_location) { return nullptr;}
    static void end(SHARED_PARAMS, ControlFlow &flow) {}
    static void br(SHARED_PARAMS, std::span<ControlFlow> control_stack,
                   uint32_t depth) {}
    static void br_if(SHARED_PARAMS, std::span<ControlFlow> control_stack,
                      uint32_t depth) {}
    static void br_table(SHARED_PARAMS, std::span<ControlFlow> control_stack,
                         std::span<uint32_t> targets) {}
    static void return_(SHARED_PARAMS, std::span<ControlFlow> control_stack) {}
    static void call_extern(SHARED_PARAMS, FunctionShell &fn,
                            uint32_t func_offset) {}
    static std::byte *call(SHARED_PARAMS, FunctionShell &fn) { return nullptr;}
    static void call_indirect(SHARED_PARAMS, uint32_t table_offset,
                              WasmSignature &type) {}
    static void drop(SHARED_PARAMS) {}
    static void select(SHARED_PARAMS) {}
    static void select_t(SHARED_PARAMS) {}
    static void localget(SHARED_PARAMS, FunctionShell &fn, uint32_t local_idx) {
    }
    static void localset(SHARED_PARAMS, FunctionShell &fn, uint32_t local_idx) {
    }
    static void localtee(SHARED_PARAMS, FunctionShell &fn, uint32_t local_idx) {
    }
    static void tableget(SHARED_PARAMS, uint64_t misc_offset) {}
    static void tableset(SHARED_PARAMS, uint64_t misc_offset) {}
    static void globalget(SHARED_PARAMS, uint64_t misc_offset) {}
    static void globalset(SHARED_PARAMS, uint64_t misc_offset) {}
    static void memorysize(SHARED_PARAMS) {}
    static void memorygrow(SHARED_PARAMS) {}
    static void i32const(SHARED_PARAMS, uint32_t cons) {}
    static void i64const(SHARED_PARAMS, uint64_t cons) {}
    static void f32const(SHARED_PARAMS, float cons) {}
    static void f64const(SHARED_PARAMS, double cons) {}
    static void i32load(SHARED_PARAMS, uint64_t offset, uint64_t align) {}
    static void i64load(SHARED_PARAMS, uint64_t offset, uint64_t align) {}
    static void f32load(SHARED_PARAMS, uint64_t offset, uint64_t align) {}
    static void f64load(SHARED_PARAMS, uint64_t offset, uint64_t align) {}
    static void i32load8_s(SHARED_PARAMS, uint64_t offset, uint64_t align) {}
    static void i32load8_u(SHARED_PARAMS, uint64_t offset, uint64_t align) {}
    static void i32load16_s(SHARED_PARAMS, uint64_t offset, uint64_t align) {}
    static void i32load16_u(SHARED_PARAMS, uint64_t offset, uint64_t align) {}
    static void i64load8_s(SHARED_PARAMS, uint64_t offset, uint64_t align) {}
    static void i64load8_u(SHARED_PARAMS, uint64_t offset, uint64_t align) {}
    static void i64load16_s(SHARED_PARAMS, uint64_t offset, uint64_t align) {}
    static void i64load16_u(SHARED_PARAMS, uint64_t offset, uint64_t align) {}
    static void i64load32_s(SHARED_PARAMS, uint64_t offset, uint64_t align) {}
    static void i64load32_u(SHARED_PARAMS, uint64_t offset, uint64_t align) {}
    static void i32store(SHARED_PARAMS, uint64_t offset, uint64_t align) {}
    static void i64store(SHARED_PARAMS, uint64_t offset, uint64_t align) {}
    static void f32store(SHARED_PARAMS, uint64_t offset, uint64_t align) {}
    static void f64store(SHARED_PARAMS, uint64_t offset, uint64_t align) {}
    static void i32store8(SHARED_PARAMS, uint64_t offset, uint64_t align) {}
    static void i32store16(SHARED_PARAMS, uint64_t offset, uint64_t align) {}
    static void i64store8(SHARED_PARAMS, uint64_t offset, uint64_t align) {}
    static void i64store16(SHARED_PARAMS, uint64_t offset, uint64_t align) {}
    static void i64store32(SHARED_PARAMS, uint64_t offset, uint64_t align) {}
    static void i32eqz(SHARED_PARAMS) {}
    static void i64eqz(SHARED_PARAMS) {}
    static void i32eq(SHARED_PARAMS) {}
    static void i64eq(SHARED_PARAMS) {}
    static void i32ne(SHARED_PARAMS) {}
    static void i64ne(SHARED_PARAMS) {}
    static void i32lt_s(SHARED_PARAMS) {}
    static void i64lt_s(SHARED_PARAMS) {}
    static void i32lt_u(SHARED_PARAMS) {}
    static void i64lt_u(SHARED_PARAMS) {}
    static void i32gt_s(SHARED_PARAMS) {}
    static void i64gt_s(SHARED_PARAMS) {}
    static void i32gt_u(SHARED_PARAMS) {}
    static void i64gt_u(SHARED_PARAMS) {}
    static void i32le_s(SHARED_PARAMS) {}
    static void i64le_s(SHARED_PARAMS) {}
    static void i32le_u(SHARED_PARAMS) {}
    static void i64le_u(SHARED_PARAMS) {}
    static void i32ge_s(SHARED_PARAMS) {}
    static void i64ge_s(SHARED_PARAMS) {}
    static void i32ge_u(SHARED_PARAMS) {}
    static void i64ge_u(SHARED_PARAMS) {}
    static void f32eq(SHARED_PARAMS) {}
    static void f64eq(SHARED_PARAMS) {}
    static void f32ne(SHARED_PARAMS) {}
    static void f64ne(SHARED_PARAMS) {}
    static void f32lt(SHARED_PARAMS) {}
    static void f64lt(SHARED_PARAMS) {}
    static void f32gt(SHARED_PARAMS) {}
    static void f64gt(SHARED_PARAMS) {}
    static void f32le(SHARED_PARAMS) {}
    static void f64le(SHARED_PARAMS) {}
    static void f32ge(SHARED_PARAMS) {}
    static void f64ge(SHARED_PARAMS) {}
    static void i32clz(SHARED_PARAMS) {}
    static void i64clz(SHARED_PARAMS) {}
    static void i32ctz(SHARED_PARAMS) {}
    static void i64ctz(SHARED_PARAMS) {}
    static void i32popcnt(SHARED_PARAMS) {}
    static void i64popcnt(SHARED_PARAMS) {}
    static void i32add(SHARED_PARAMS) {}
    static void i64add(SHARED_PARAMS) {}
    static void i32sub(SHARED_PARAMS) {}
    static void i64sub(SHARED_PARAMS) {}
    static void i32mul(SHARED_PARAMS) {}
    static void i64mul(SHARED_PARAMS) {}
    static void i32div_s(SHARED_PARAMS) {}
    static void i64div_s(SHARED_PARAMS) {}
    static void i32div_u(SHARED_PARAMS) {}
    static void i64div_u(SHARED_PARAMS) {}
    static void i32rem_s(SHARED_PARAMS) {}
    static void i64rem_s(SHARED_PARAMS) {}
    static void i32rem_u(SHARED_PARAMS) {}
    static void i64rem_u(SHARED_PARAMS) {}
    static void i32and(SHARED_PARAMS) {}
    static void i64and(SHARED_PARAMS) {}
    static void i32or(SHARED_PARAMS) {}
    static void i64or(SHARED_PARAMS) {}
    static void i32xor(SHARED_PARAMS) {}
    static void i64xor(SHARED_PARAMS) {}
    static void i32shl(SHARED_PARAMS) {}
    static void i64shl(SHARED_PARAMS) {}
    static void i32shr_s(SHARED_PARAMS) {}
    static void i64shr_s(SHARED_PARAMS) {}
    static void i32shr_u(SHARED_PARAMS) {}
    static void i64shr_u(SHARED_PARAMS) {}
    static void i32rotl(SHARED_PARAMS) {}
    static void i64rotl(SHARED_PARAMS) {}
    static void i32rotr(SHARED_PARAMS) {}
    static void i64rotr(SHARED_PARAMS) {}
    static void f32abs(SHARED_PARAMS) {}
    static void f64abs(SHARED_PARAMS) {}
    static void f32neg(SHARED_PARAMS) {}
    static void f64neg(SHARED_PARAMS) {}
    static void f32ceil(SHARED_PARAMS) {}
    static void f64ceil(SHARED_PARAMS) {}
    static void f32floor(SHARED_PARAMS) {}
    static void f64floor(SHARED_PARAMS) {}
    static void f32trunc(SHARED_PARAMS) {}
    static void f64trunc(SHARED_PARAMS) {}
    static void f32nearest(SHARED_PARAMS) {}
    static void f64nearest(SHARED_PARAMS) {}
    static void f32sqrt(SHARED_PARAMS) {}
    static void f64sqrt(SHARED_PARAMS) {}
    static void f32add(SHARED_PARAMS) {}
    static void f64add(SHARED_PARAMS) {}
    static void f32sub(SHARED_PARAMS) {}
    static void f64sub(SHARED_PARAMS) {}
    static void f32mul(SHARED_PARAMS) {}
    static void f64mul(SHARED_PARAMS) {}
    static void f32div(SHARED_PARAMS) {}
    static void f64div(SHARED_PARAMS) {}
    static void f32min(SHARED_PARAMS) {}
    static void f64min(SHARED_PARAMS) {}
    static void f32max(SHARED_PARAMS) {}
    static void f64max(SHARED_PARAMS) {}
    static void f32copysign(SHARED_PARAMS) {}
    static void f64copysign(SHARED_PARAMS) {}
    static void i32wrap_i64(SHARED_PARAMS) {}
    static void i64extend_i32_s(SHARED_PARAMS) {}
    static void i64extend_i32_u(SHARED_PARAMS) {}
    static void i32trunc_f32_s(SHARED_PARAMS) {}
    static void i64trunc_f32_s(SHARED_PARAMS) {}
    static void i32trunc_f32_u(SHARED_PARAMS) {}
    static void i64trunc_f32_u(SHARED_PARAMS) {}
    static void i32trunc_f64_s(SHARED_PARAMS) {}
    static void i64trunc_f64_s(SHARED_PARAMS) {}
    static void i32trunc_f64_u(SHARED_PARAMS) {}
    static void i64trunc_f64_u(SHARED_PARAMS) {}
    static void f32convert_i32_s(SHARED_PARAMS) {}
    static void f64convert_i32_s(SHARED_PARAMS) {}
    static void f32convert_i32_u(SHARED_PARAMS) {}
    static void f64convert_i32_u(SHARED_PARAMS) {}
    static void f32convert_i64_s(SHARED_PARAMS) {}
    static void f64convert_i64_s(SHARED_PARAMS) {}
    static void f32convert_i64_u(SHARED_PARAMS) {}
    static void f64convert_i64_u(SHARED_PARAMS) {}
    static void f32demote_f64(SHARED_PARAMS) {}
    static void f64promote_f32(SHARED_PARAMS) {}
    static void i32reinterpret_f32(SHARED_PARAMS) {}
    static void f32reinterpret_i32(SHARED_PARAMS) {}
    static void i64reinterpret_f64(SHARED_PARAMS) {}
    static void f64reinterpret_i64(SHARED_PARAMS) {}
    static void i32extend8_s(SHARED_PARAMS) {}
    static void i32extend16_s(SHARED_PARAMS) {}
    static void i64extend8_s(SHARED_PARAMS) {}
    static void i64extend16_s(SHARED_PARAMS) {}
    static void i64extend32_s(SHARED_PARAMS) {}
    static void ref_null(SHARED_PARAMS) {}
    static void ref_is_null(SHARED_PARAMS) {}
    static void ref_func(SHARED_PARAMS, uint64_t misc_offset) {}
    static void ref_eq(SHARED_PARAMS) {}
    static void i32_trunc_sat_f32_s(SHARED_PARAMS) {}
    static void i32_trunc_sat_f32_u(SHARED_PARAMS) {}
    static void i32_trunc_sat_f64_s(SHARED_PARAMS) {}
    static void i32_trunc_sat_f64_u(SHARED_PARAMS) {}
    static void i64_trunc_sat_f32_s(SHARED_PARAMS) {}
    static void i64_trunc_sat_f32_u(SHARED_PARAMS) {}
    static void i64_trunc_sat_f64_s(SHARED_PARAMS) {}
    static void i64_trunc_sat_f64_u(SHARED_PARAMS) {}
    static void memory_init(SHARED_PARAMS, uint64_t misc_offset) {}
    static void data_drop(SHARED_PARAMS, uint64_t misc_offset) {}
    static void memory_copy(SHARED_PARAMS) {}
    static void memory_fill(SHARED_PARAMS) {}
    static void table_init(SHARED_PARAMS, uint64_t seg_offset,
                           uint64_t table_offset) {}
    static void elem_drop(SHARED_PARAMS, uint64_t misc_offset) {}
    static void table_copy(SHARED_PARAMS, uint64_t dst_offset,
                           uint64_t src_offset) {}
    static void table_grow(SHARED_PARAMS, uint64_t misc_offset) {}
    static void table_size(SHARED_PARAMS, uint64_t misc_offset) {}
    static void table_fill(SHARED_PARAMS, uint64_t misc_offset) {}
};

}; // namespace mitey