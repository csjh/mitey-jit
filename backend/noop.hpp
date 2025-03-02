#include "../module.hpp"

#define SHARED_PARAMS std::byte *&, WasmStack &

namespace mitey {

class Noop {
  public:
    static constexpr size_t function_overhead = 1;
    static constexpr size_t max_instruction = 1;

    void start_function(SHARED_PARAMS, FunctionShell &) {}
    void exit_function(SHARED_PARAMS, FunctionShell &) {}

    void unreachable(SHARED_PARAMS) {}
    void nop(SHARED_PARAMS) {}
    void block(SHARED_PARAMS, WasmSignature &) {}
    void loop(SHARED_PARAMS, WasmSignature &) {}
    std::byte *if_(SHARED_PARAMS, WasmSignature &) { return nullptr; }
    std::byte *else_(SHARED_PARAMS, WasmSignature &, std::byte *) {
        return nullptr;
    }
    void end(SHARED_PARAMS, ControlFlow &) {}
    void br(SHARED_PARAMS, std::span<ControlFlow>, uint32_t) {}
    void br_if(SHARED_PARAMS, std::span<ControlFlow>, uint32_t) {}
    void br_table(SHARED_PARAMS, std::span<ControlFlow>, std::span<uint32_t>) {}
    void return_(SHARED_PARAMS, std::span<ControlFlow>) {}
    std::byte *call(SHARED_PARAMS, FunctionShell &) { return nullptr; }
    void call_indirect(SHARED_PARAMS, uint32_t, WasmSignature &) {}
    void drop(SHARED_PARAMS) {}
    void select(SHARED_PARAMS) {}
    void select_t(SHARED_PARAMS) {}
    void localget(SHARED_PARAMS, FunctionShell &, uint32_t) {}
    void localset(SHARED_PARAMS, FunctionShell &, uint32_t) {}
    void localtee(SHARED_PARAMS, FunctionShell &, uint32_t) {}
    void tableget(SHARED_PARAMS, uint64_t) {}
    void tableset(SHARED_PARAMS, uint64_t) {}
    void globalget(SHARED_PARAMS, uint64_t) {}
    void globalset(SHARED_PARAMS, uint64_t) {}
    void memorysize(SHARED_PARAMS) {}
    void memorygrow(SHARED_PARAMS) {}
    void i32const(SHARED_PARAMS, uint32_t) {}
    void i64const(SHARED_PARAMS, uint64_t) {}
    void f32const(SHARED_PARAMS, float) {}
    void f64const(SHARED_PARAMS, double) {}
    void i32load(SHARED_PARAMS, uint64_t, uint64_t) {}
    void i64load(SHARED_PARAMS, uint64_t, uint64_t) {}
    void f32load(SHARED_PARAMS, uint64_t, uint64_t) {}
    void f64load(SHARED_PARAMS, uint64_t, uint64_t) {}
    void i32load8_s(SHARED_PARAMS, uint64_t, uint64_t) {}
    void i32load8_u(SHARED_PARAMS, uint64_t, uint64_t) {}
    void i32load16_s(SHARED_PARAMS, uint64_t, uint64_t) {}
    void i32load16_u(SHARED_PARAMS, uint64_t, uint64_t) {}
    void i64load8_s(SHARED_PARAMS, uint64_t, uint64_t) {}
    void i64load8_u(SHARED_PARAMS, uint64_t, uint64_t) {}
    void i64load16_s(SHARED_PARAMS, uint64_t, uint64_t) {}
    void i64load16_u(SHARED_PARAMS, uint64_t, uint64_t) {}
    void i64load32_s(SHARED_PARAMS, uint64_t, uint64_t) {}
    void i64load32_u(SHARED_PARAMS, uint64_t, uint64_t) {}
    void i32store(SHARED_PARAMS, uint64_t, uint64_t) {}
    void i64store(SHARED_PARAMS, uint64_t, uint64_t) {}
    void f32store(SHARED_PARAMS, uint64_t, uint64_t) {}
    void f64store(SHARED_PARAMS, uint64_t, uint64_t) {}
    void i32store8(SHARED_PARAMS, uint64_t, uint64_t) {}
    void i32store16(SHARED_PARAMS, uint64_t, uint64_t) {}
    void i64store8(SHARED_PARAMS, uint64_t, uint64_t) {}
    void i64store16(SHARED_PARAMS, uint64_t, uint64_t) {}
    void i64store32(SHARED_PARAMS, uint64_t, uint64_t) {}
    void i32eqz(SHARED_PARAMS) {}
    void i64eqz(SHARED_PARAMS) {}
    void i32eq(SHARED_PARAMS) {}
    void i64eq(SHARED_PARAMS) {}
    void i32ne(SHARED_PARAMS) {}
    void i64ne(SHARED_PARAMS) {}
    void i32lt_s(SHARED_PARAMS) {}
    void i64lt_s(SHARED_PARAMS) {}
    void i32lt_u(SHARED_PARAMS) {}
    void i64lt_u(SHARED_PARAMS) {}
    void i32gt_s(SHARED_PARAMS) {}
    void i64gt_s(SHARED_PARAMS) {}
    void i32gt_u(SHARED_PARAMS) {}
    void i64gt_u(SHARED_PARAMS) {}
    void i32le_s(SHARED_PARAMS) {}
    void i64le_s(SHARED_PARAMS) {}
    void i32le_u(SHARED_PARAMS) {}
    void i64le_u(SHARED_PARAMS) {}
    void i32ge_s(SHARED_PARAMS) {}
    void i64ge_s(SHARED_PARAMS) {}
    void i32ge_u(SHARED_PARAMS) {}
    void i64ge_u(SHARED_PARAMS) {}
    void f32eq(SHARED_PARAMS) {}
    void f64eq(SHARED_PARAMS) {}
    void f32ne(SHARED_PARAMS) {}
    void f64ne(SHARED_PARAMS) {}
    void f32lt(SHARED_PARAMS) {}
    void f64lt(SHARED_PARAMS) {}
    void f32gt(SHARED_PARAMS) {}
    void f64gt(SHARED_PARAMS) {}
    void f32le(SHARED_PARAMS) {}
    void f64le(SHARED_PARAMS) {}
    void f32ge(SHARED_PARAMS) {}
    void f64ge(SHARED_PARAMS) {}
    void i32clz(SHARED_PARAMS) {}
    void i64clz(SHARED_PARAMS) {}
    void i32ctz(SHARED_PARAMS) {}
    void i64ctz(SHARED_PARAMS) {}
    void i32popcnt(SHARED_PARAMS) {}
    void i64popcnt(SHARED_PARAMS) {}
    void i32add(SHARED_PARAMS) {}
    void i64add(SHARED_PARAMS) {}
    void i32sub(SHARED_PARAMS) {}
    void i64sub(SHARED_PARAMS) {}
    void i32mul(SHARED_PARAMS) {}
    void i64mul(SHARED_PARAMS) {}
    void i32div_s(SHARED_PARAMS) {}
    void i64div_s(SHARED_PARAMS) {}
    void i32div_u(SHARED_PARAMS) {}
    void i64div_u(SHARED_PARAMS) {}
    void i32rem_s(SHARED_PARAMS) {}
    void i64rem_s(SHARED_PARAMS) {}
    void i32rem_u(SHARED_PARAMS) {}
    void i64rem_u(SHARED_PARAMS) {}
    void i32and(SHARED_PARAMS) {}
    void i64and(SHARED_PARAMS) {}
    void i32or(SHARED_PARAMS) {}
    void i64or(SHARED_PARAMS) {}
    void i32xor(SHARED_PARAMS) {}
    void i64xor(SHARED_PARAMS) {}
    void i32shl(SHARED_PARAMS) {}
    void i64shl(SHARED_PARAMS) {}
    void i32shr_s(SHARED_PARAMS) {}
    void i64shr_s(SHARED_PARAMS) {}
    void i32shr_u(SHARED_PARAMS) {}
    void i64shr_u(SHARED_PARAMS) {}
    void i32rotl(SHARED_PARAMS) {}
    void i64rotl(SHARED_PARAMS) {}
    void i32rotr(SHARED_PARAMS) {}
    void i64rotr(SHARED_PARAMS) {}
    void f32abs(SHARED_PARAMS) {}
    void f64abs(SHARED_PARAMS) {}
    void f32neg(SHARED_PARAMS) {}
    void f64neg(SHARED_PARAMS) {}
    void f32ceil(SHARED_PARAMS) {}
    void f64ceil(SHARED_PARAMS) {}
    void f32floor(SHARED_PARAMS) {}
    void f64floor(SHARED_PARAMS) {}
    void f32trunc(SHARED_PARAMS) {}
    void f64trunc(SHARED_PARAMS) {}
    void f32nearest(SHARED_PARAMS) {}
    void f64nearest(SHARED_PARAMS) {}
    void f32sqrt(SHARED_PARAMS) {}
    void f64sqrt(SHARED_PARAMS) {}
    void f32add(SHARED_PARAMS) {}
    void f64add(SHARED_PARAMS) {}
    void f32sub(SHARED_PARAMS) {}
    void f64sub(SHARED_PARAMS) {}
    void f32mul(SHARED_PARAMS) {}
    void f64mul(SHARED_PARAMS) {}
    void f32div(SHARED_PARAMS) {}
    void f64div(SHARED_PARAMS) {}
    void f32min(SHARED_PARAMS) {}
    void f64min(SHARED_PARAMS) {}
    void f32max(SHARED_PARAMS) {}
    void f64max(SHARED_PARAMS) {}
    void f32copysign(SHARED_PARAMS) {}
    void f64copysign(SHARED_PARAMS) {}
    void i32wrap_i64(SHARED_PARAMS) {}
    void i64extend_i32_s(SHARED_PARAMS) {}
    void i64extend_i32_u(SHARED_PARAMS) {}
    void i32trunc_f32_s(SHARED_PARAMS) {}
    void i64trunc_f32_s(SHARED_PARAMS) {}
    void i32trunc_f32_u(SHARED_PARAMS) {}
    void i64trunc_f32_u(SHARED_PARAMS) {}
    void i32trunc_f64_s(SHARED_PARAMS) {}
    void i64trunc_f64_s(SHARED_PARAMS) {}
    void i32trunc_f64_u(SHARED_PARAMS) {}
    void i64trunc_f64_u(SHARED_PARAMS) {}
    void f32convert_i32_s(SHARED_PARAMS) {}
    void f64convert_i32_s(SHARED_PARAMS) {}
    void f32convert_i32_u(SHARED_PARAMS) {}
    void f64convert_i32_u(SHARED_PARAMS) {}
    void f32convert_i64_s(SHARED_PARAMS) {}
    void f64convert_i64_s(SHARED_PARAMS) {}
    void f32convert_i64_u(SHARED_PARAMS) {}
    void f64convert_i64_u(SHARED_PARAMS) {}
    void f32demote_f64(SHARED_PARAMS) {}
    void f64promote_f32(SHARED_PARAMS) {}
    void i32reinterpret_f32(SHARED_PARAMS) {}
    void f32reinterpret_i32(SHARED_PARAMS) {}
    void i64reinterpret_f64(SHARED_PARAMS) {}
    void f64reinterpret_i64(SHARED_PARAMS) {}
    void i32extend8_s(SHARED_PARAMS) {}
    void i32extend16_s(SHARED_PARAMS) {}
    void i64extend8_s(SHARED_PARAMS) {}
    void i64extend16_s(SHARED_PARAMS) {}
    void i64extend32_s(SHARED_PARAMS) {}
    void ref_null(SHARED_PARAMS) {}
    void ref_is_null(SHARED_PARAMS) {}
    void ref_func(SHARED_PARAMS, uint64_t) {}
    void ref_eq(SHARED_PARAMS) {}
    void i32_trunc_sat_f32_s(SHARED_PARAMS) {}
    void i32_trunc_sat_f32_u(SHARED_PARAMS) {}
    void i32_trunc_sat_f64_s(SHARED_PARAMS) {}
    void i32_trunc_sat_f64_u(SHARED_PARAMS) {}
    void i64_trunc_sat_f32_s(SHARED_PARAMS) {}
    void i64_trunc_sat_f32_u(SHARED_PARAMS) {}
    void i64_trunc_sat_f64_s(SHARED_PARAMS) {}
    void i64_trunc_sat_f64_u(SHARED_PARAMS) {}
    void memory_init(SHARED_PARAMS, uint64_t) {}
    void data_drop(SHARED_PARAMS, uint64_t) {}
    void memory_copy(SHARED_PARAMS) {}
    void memory_fill(SHARED_PARAMS) {}
    void table_init(SHARED_PARAMS, uint64_t, uint64_t) {}
    void elem_drop(SHARED_PARAMS, uint64_t) {}
    void table_copy(SHARED_PARAMS, uint64_t, uint64_t) {}
    void table_grow(SHARED_PARAMS, uint64_t) {}
    void table_size(SHARED_PARAMS, uint64_t) {}
    void table_fill(SHARED_PARAMS, uint64_t) {}
};

}; // namespace mitey