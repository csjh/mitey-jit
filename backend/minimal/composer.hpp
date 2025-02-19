#include "../../runtime.hpp"
#include <cstdint>
#include <cstring>

namespace mitey {

#define SHARED_PARAMS uint8_t *&code

template <typename Target> class Composer {
    // unused
    using extra = int;

    template <runtime::Signature func> static void nilary(SHARED_PARAMS) {
        Target::put_call(code, func);
    }

    template <runtime::Signature func>
    static void unary(SHARED_PARAMS, uint64_t tmp1) {
        Target::put_temp1(code, tmp1);
        Target::put_call(code, func);
    }

    template <runtime::Signature func>
    static void binary(SHARED_PARAMS, uint64_t tmp1, uint64_t tmp2) {
        Target::put_temp1(code, tmp1);
        Target::put_temp2(code, tmp2);
        Target::put_call(code, func);
    }

    template <runtime::Signature func>
    static void memop(SHARED_PARAMS, uint64_t offset, uint64_t align) {
        Target::put_temp1(code, offset);
        Target::put_call(code, func);
    }

  public:
    static void enter_function(SHARED_PARAMS) { Target::put_prelude(code); }
    static void exit_function(SHARED_PARAMS) { Target::put_postlude(code); }

    static auto unreachable = nilary<runtime::unreachable>;
    static void nop(SHARED_PARAMS) {}
    static void block(SHARED_PARAMS) { Target::put_call(code, runtime::block); }
    static void loop(SHARED_PARAMS) { Target::put_call(code, runtime::loop); }
    static void if_(SHARED_PARAMS) { Target::put_call(code, runtime::if_); }
    static void else_(SHARED_PARAMS) { Target::put_call(code, runtime::else_); }
    static void end(SHARED_PARAMS) { Target::put_call(code, runtime::end); }
    static void br(SHARED_PARAMS) { Target::put_call(code, runtime::br); }
    static void br_if(SHARED_PARAMS) { Target::put_call(code, runtime::br_if); }
    static void br_table(SHARED_PARAMS) {
        Target::put_call(code, runtime::br_table);
    }
    static void return_(SHARED_PARAMS) {
        Target::put_call(code, runtime::return_);
    }
    static void call(SHARED_PARAMS) { Target::put_call(code, runtime::call); }
    static void call_indirect(SHARED_PARAMS) {
        Target::put_call(code, runtime::call_indirect);
    }
    static void drop(SHARED_PARAMS) { Target::put_call(code, runtime::drop); }
    static void select(SHARED_PARAMS) {
        Target::put_call(code, runtime::select);
    }
    static void select_t(SHARED_PARAMS) {
        Target::put_call(code, runtime::select_t);
    }
    static void localget(SHARED_PARAMS) {
        Target::put_call(code, runtime::localget);
    }
    static void localset(SHARED_PARAMS) {
        Target::put_call(code, runtime::localset);
    }
    static void localtee(SHARED_PARAMS) {
        Target::put_call(code, runtime::localtee);
    }
    static auto tableget = unary<runtime::tableget>;
    static auto tableset = unary<runtime::tableset>;
    static auto globalget = unary<runtime::globalget>;
    static auto globalset = unary<runtime::globalset>;
    static auto memorysize = nilary<runtime::memorysize>;
    static auto memorygrow = nilary<runtime::memorygrow>;
    static void i32const(SHARED_PARAMS, uint32_t cons) {
        runtime::WasmValue v;
        std::memcpy(&v.i32, &cons, sizeof(uint32_t));
        Target::put_temp1(code, v.u64);
        Target::put_call(code, runtime::i32const);
    }
    static void i64const(SHARED_PARAMS, uint64_t cons) {
        runtime::WasmValue v;
        std::memcpy(&v.i64, &cons, sizeof(uint32_t));
        Target::put_temp1(code, v.u64);
        Target::put_call(code, runtime::i64const);
    }
    static void f32const(SHARED_PARAMS, float cons) {
        runtime::WasmValue v;
        std::memcpy(&v.f32, &cons, sizeof(uint32_t));
        Target::put_temp1(code, v.u64);
        Target::put_call(code, runtime::f32const);
    }
    static void f64const(SHARED_PARAMS, double cons) {
        runtime::WasmValue v;
        std::memcpy(&v.f64, &cons, sizeof(uint32_t));
        Target::put_temp1(code, v.u64);
        Target::put_call(code, runtime::f64const);
    }
    static auto i32load = memop<runtime::i32load>;
    static auto i64load = memop<runtime::i64load>;
    static auto f32load = memop<runtime::f32load>;
    static auto f64load = memop<runtime::f64load>;
    static auto i32load8_s = memop<runtime::i32load8_s>;
    static auto i32load8_u = memop<runtime::i32load8_u>;
    static auto i32load16_s = memop<runtime::i32load16_s>;
    static auto i32load16_u = memop<runtime::i32load16_u>;
    static auto i64load8_s = memop<runtime::i64load8_s>;
    static auto i64load8_u = memop<runtime::i64load8_u>;
    static auto i64load16_s = memop<runtime::i64load16_s>;
    static auto i64load16_u = memop<runtime::i64load16_u>;
    static auto i64load32_s = memop<runtime::i64load32_s>;
    static auto i64load32_u = memop<runtime::i64load32_u>;
    static auto i32store = memop<runtime::i32store>;
    static auto i64store = memop<runtime::i64store>;
    static auto f32store = memop<runtime::f32store>;
    static auto f64store = memop<runtime::f64store>;
    static auto i32store8 = memop<runtime::i32store8>;
    static auto i32store16 = memop<runtime::i32store16>;
    static auto i64store8 = memop<runtime::i64store8>;
    static auto i64store16 = memop<runtime::i64store16>;
    static auto i64store32 = memop<runtime::i64store32>;
    static auto i32eqz = nilary<runtime::i32eqz>;
    static auto i64eqz = nilary<runtime::i64eqz>;
    static auto i32eq = nilary<runtime::i32eq>;
    static auto i64eq = nilary<runtime::i64eq>;
    static auto i32ne = nilary<runtime::i32ne>;
    static auto i64ne = nilary<runtime::i64ne>;
    static auto i32lt_s = nilary<runtime::i32lt_s>;
    static auto i64lt_s = nilary<runtime::i64lt_s>;
    static auto i32lt_u = nilary<runtime::i32lt_u>;
    static auto i64lt_u = nilary<runtime::i64lt_u>;
    static auto i32gt_s = nilary<runtime::i32gt_s>;
    static auto i64gt_s = nilary<runtime::i64gt_s>;
    static auto i32gt_u = nilary<runtime::i32gt_u>;
    static auto i64gt_u = nilary<runtime::i64gt_u>;
    static auto i32le_s = nilary<runtime::i32le_s>;
    static auto i64le_s = nilary<runtime::i64le_s>;
    static auto i32le_u = nilary<runtime::i32le_u>;
    static auto i64le_u = nilary<runtime::i64le_u>;
    static auto i32ge_s = nilary<runtime::i32ge_s>;
    static auto i64ge_s = nilary<runtime::i64ge_s>;
    static auto i32ge_u = nilary<runtime::i32ge_u>;
    static auto i64ge_u = nilary<runtime::i64ge_u>;
    static auto f32eq = nilary<runtime::f32eq>;
    static auto f64eq = nilary<runtime::f64eq>;
    static auto f32ne = nilary<runtime::f32ne>;
    static auto f64ne = nilary<runtime::f64ne>;
    static auto f32lt = nilary<runtime::f32lt>;
    static auto f64lt = nilary<runtime::f64lt>;
    static auto f32gt = nilary<runtime::f32gt>;
    static auto f64gt = nilary<runtime::f64gt>;
    static auto f32le = nilary<runtime::f32le>;
    static auto f64le = nilary<runtime::f64le>;
    static auto f32ge = nilary<runtime::f32ge>;
    static auto f64ge = nilary<runtime::f64ge>;
    static auto i32clz = nilary<runtime::i32clz>;
    static auto i64clz = nilary<runtime::i64clz>;
    static auto i32ctz = nilary<runtime::i32ctz>;
    static auto i64ctz = nilary<runtime::i64ctz>;
    static auto i32popcnt = nilary<runtime::i32popcnt>;
    static auto i64popcnt = nilary<runtime::i64popcnt>;
    static auto i32add = nilary<runtime::i32add>;
    static auto i64add = nilary<runtime::i64add>;
    static auto i32sub = nilary<runtime::i32sub>;
    static auto i64sub = nilary<runtime::i64sub>;
    static auto i32mul = nilary<runtime::i32mul>;
    static auto i64mul = nilary<runtime::i64mul>;
    static auto i32div_s = nilary<runtime::i32div_s>;
    static auto i64div_s = nilary<runtime::i64div_s>;
    static auto i32div_u = nilary<runtime::i32div_u>;
    static auto i64div_u = nilary<runtime::i64div_u>;
    static auto i32rem_s = nilary<runtime::i32rem_s>;
    static auto i64rem_s = nilary<runtime::i64rem_s>;
    static auto i32rem_u = nilary<runtime::i32rem_u>;
    static auto i64rem_u = nilary<runtime::i64rem_u>;
    static auto i32and = nilary<runtime::i32and>;
    static auto i64and = nilary<runtime::i64and>;
    static auto i32or = nilary<runtime::i32or>;
    static auto i64or = nilary<runtime::i64or>;
    static auto i32xor = nilary<runtime::i32xor>;
    static auto i64xor = nilary<runtime::i64xor>;
    static auto i32shl = nilary<runtime::i32shl>;
    static auto i64shl = nilary<runtime::i64shl>;
    static auto i32shr_s = nilary<runtime::i32shr_s>;
    static auto i64shr_s = nilary<runtime::i64shr_s>;
    static auto i32shr_u = nilary<runtime::i32shr_u>;
    static auto i64shr_u = nilary<runtime::i64shr_u>;
    static auto i32rotl = nilary<runtime::i32rotl>;
    static auto i64rotl = nilary<runtime::i64rotl>;
    static auto i32rotr = nilary<runtime::i32rotr>;
    static auto i64rotr = nilary<runtime::i64rotr>;
    static auto f32abs = nilary<runtime::f32abs>;
    static auto f64abs = nilary<runtime::f64abs>;
    static auto f32neg = nilary<runtime::f32neg>;
    static auto f64neg = nilary<runtime::f64neg>;
    static auto f32ceil = nilary<runtime::f32ceil>;
    static auto f64ceil = nilary<runtime::f64ceil>;
    static auto f32floor = nilary<runtime::f32floor>;
    static auto f64floor = nilary<runtime::f64floor>;
    static auto f32trunc = nilary<runtime::f32trunc>;
    static auto f64trunc = nilary<runtime::f64trunc>;
    static auto f32nearest = nilary<runtime::f32nearest>;
    static auto f64nearest = nilary<runtime::f64nearest>;
    static auto f32sqrt = nilary<runtime::f32sqrt>;
    static auto f64sqrt = nilary<runtime::f64sqrt>;
    static auto f32add = nilary<runtime::f32add>;
    static auto f64add = nilary<runtime::f64add>;
    static auto f32sub = nilary<runtime::f32sub>;
    static auto f64sub = nilary<runtime::f64sub>;
    static auto f32mul = nilary<runtime::f32mul>;
    static auto f64mul = nilary<runtime::f64mul>;
    static auto f32div = nilary<runtime::f32div>;
    static auto f64div = nilary<runtime::f64div>;
    static auto f32min = nilary<runtime::f32min>;
    static auto f64min = nilary<runtime::f64min>;
    static auto f32max = nilary<runtime::f32max>;
    static auto f64max = nilary<runtime::f64max>;
    static auto f32copysign = nilary<runtime::f32copysign>;
    static auto f64copysign = nilary<runtime::f64copysign>;
    static auto i32wrap_i64 = nilary<runtime::i32wrap_i64>;
    static auto i64extend_i32_s = nilary<runtime::i64extend_i32_s>;
    static auto i64extend_i32_u = nilary<runtime::i64extend_i32_u>;
    static auto i32trunc_f32_s = nilary<runtime::i32trunc_f32_s>;
    static auto i64trunc_f32_s = nilary<runtime::i64trunc_f32_s>;
    static auto i32trunc_f32_u = nilary<runtime::i32trunc_f32_u>;
    static auto i64trunc_f32_u = nilary<runtime::i64trunc_f32_u>;
    static auto i32trunc_f64_s = nilary<runtime::i32trunc_f64_s>;
    static auto i64trunc_f64_s = nilary<runtime::i64trunc_f64_s>;
    static auto i32trunc_f64_u = nilary<runtime::i32trunc_f64_u>;
    static auto i64trunc_f64_u = nilary<runtime::i64trunc_f64_u>;
    static auto f32convert_i32_s = nilary<runtime::f32convert_i32_s>;
    static auto f64convert_i32_s = nilary<runtime::f64convert_i32_s>;
    static auto f32convert_i32_u = nilary<runtime::f32convert_i32_u>;
    static auto f64convert_i32_u = nilary<runtime::f64convert_i32_u>;
    static auto f32convert_i64_s = nilary<runtime::f32convert_i64_s>;
    static auto f64convert_i64_s = nilary<runtime::f64convert_i64_s>;
    static auto f32convert_i64_u = nilary<runtime::f32convert_i64_u>;
    static auto f64convert_i64_u = nilary<runtime::f64convert_i64_u>;
    static auto f32demote_f64 = nilary<runtime::f32demote_f64>;
    static auto f64promote_f32 = nilary<runtime::f64promote_f32>;
    static auto i32reinterpret_f32 = nilary<runtime::i32reinterpret_f32>;
    static auto f32reinterpret_i32 = nilary<runtime::f32reinterpret_i32>;
    static auto i64reinterpret_f64 = nilary<runtime::i64reinterpret_f64>;
    static auto f64reinterpret_i64 = nilary<runtime::f64reinterpret_i64>;
    static auto i32extend8_s = nilary<runtime::i32extend8_s>;
    static auto i32extend16_s = nilary<runtime::i32extend16_s>;
    static auto i64extend8_s = nilary<runtime::i64extend8_s>;
    static auto i64extend16_s = nilary<runtime::i64extend16_s>;
    static auto i64extend32_s = nilary<runtime::i64extend32_s>;
    static auto ref_null = nilary<runtime::ref_null>;
    static auto ref_is_null = nilary<runtime::ref_is_null>;
    static auto ref_func = unary<runtime::ref_func>;
    static auto ref_eq = nilary<runtime::ref_eq>;
    static auto i32_trunc_sat_f32_s = nilary<runtime::i32_trunc_sat_f32_s>;
    static auto i32_trunc_sat_f32_u = nilary<runtime::i32_trunc_sat_f32_u>;
    static auto i32_trunc_sat_f64_s = nilary<runtime::i32_trunc_sat_f64_s>;
    static auto i32_trunc_sat_f64_u = nilary<runtime::i32_trunc_sat_f64_u>;
    static auto i64_trunc_sat_f32_s = nilary<runtime::i64_trunc_sat_f32_s>;
    static auto i64_trunc_sat_f32_u = nilary<runtime::i64_trunc_sat_f32_u>;
    static auto i64_trunc_sat_f64_s = nilary<runtime::i64_trunc_sat_f64_s>;
    static auto i64_trunc_sat_f64_u = nilary<runtime::i64_trunc_sat_f64_u>;
    static auto memory_init = unary<runtime::memory_init>;
    static auto data_drop = unary<runtime::data_drop>;
    static auto memory_copy = nilary<runtime::memory_copy>;
    static auto memory_fill = nilary<runtime::memory_fill>;
    static auto table_init = binary<runtime::table_init>;
    static auto elem_drop = unary<runtime::elem_drop>;
    static auto table_copy = binary<runtime::table_copy>;
    static auto table_grow = unary<runtime::table_grow>;
    static auto table_size = unary<runtime::table_size>;
    static auto table_fill = unary<runtime::table_fill>;
    static void multibyte(SHARED_PARAMS) {}
};

} // namespace mitey