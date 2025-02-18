#include "../../runtime.hpp"
#include <cstdint>
#include <cstring>

namespace mitey {

namespace {
template <typename T> void put(uint8_t *&code, const T &value) {
    std::memcpy(code, &value, sizeof(T));
    code += sizeof(T);
}
} // namespace

template <typename Target> class Composer {
    // unused
    using extra = int;

    static constexpr void unreachable(uint8_t *&code) {
        Target::put_call(code, runtime::unreachable);
    }
    static constexpr void nop(uint8_t *&code) {}
    static constexpr void block(uint8_t *&code) {
        Target::put_call(code, runtime::block);
    }
    static constexpr void loop(uint8_t *&code) {
        Target::put_call(code, runtime::loop);
    }
    static constexpr void if_(uint8_t *&code) {
        Target::put_call(code, runtime::if_);
    }
    static constexpr void else_(uint8_t *&code) {
        Target::put_call(code, runtime::else_);
    }
    static constexpr void end(uint8_t *&code) {
        Target::put_call(code, runtime::end);
    }
    static constexpr void br(uint8_t *&code) {
        Target::put_call(code, runtime::br);
    }
    static constexpr void br_if(uint8_t *&code) {
        Target::put_call(code, runtime::br_if);
    }
    static constexpr void br_table(uint8_t *&code) {
        Target::put_call(code, runtime::br_table);
    }
    static constexpr void return_(uint8_t *&code) {
        Target::put_call(code, runtime::return_);
    }
    static constexpr void call(uint8_t *&code) {
        Target::put_call(code, runtime::call);
    }
    static constexpr void call_indirect(uint8_t *&code) {
        Target::put_call(code, runtime::call_indirect);
    }
    static constexpr void drop(uint8_t *&code) {
        Target::put_call(code, runtime::drop);
    }
    static constexpr void select(uint8_t *&code) {
        Target::put_call(code, runtime::select);
    }
    static constexpr void select_t(uint8_t *&code) {
        Target::put_call(code, runtime::select_t);
    }
    static constexpr void localget(uint8_t *&code) {
        Target::put_call(code, runtime::localget);
    }
    static constexpr void localset(uint8_t *&code) {
        Target::put_call(code, runtime::localset);
    }
    static constexpr void localtee(uint8_t *&code) {
        Target::put_call(code, runtime::localtee);
    }
    static constexpr void tableget(uint8_t *&code) {
        Target::put_call(code, runtime::tableget);
    }
    static constexpr void tableset(uint8_t *&code) {
        Target::put_call(code, runtime::tableset);
    }
    static constexpr void globalget(uint8_t *&code) {
        Target::put_call(code, runtime::globalget);
    }
    static constexpr void globalset(uint8_t *&code) {
        Target::put_call(code, runtime::globalset);
    }
    static constexpr void memorysize(uint8_t *&code) {
        Target::put_call(code, runtime::memorysize);
    }
    static constexpr void memorygrow(uint8_t *&code) {
        Target::put_call(code, runtime::memorygrow);
    }
    static constexpr void i32const(uint8_t *&code) {
        Target::put_call(code, runtime::i32const);
    }
    static constexpr void i64const(uint8_t *&code) {
        Target::put_call(code, runtime::i64const);
    }
    static constexpr void f32const(uint8_t *&code) {
        Target::put_call(code, runtime::f32const);
    }
    static constexpr void f64const(uint8_t *&code) {
        Target::put_call(code, runtime::f64const);
    }
    static constexpr void i32load(uint8_t *&code) {
        Target::put_call(code, runtime::i32load);
    }
    static constexpr void i64load(uint8_t *&code) {
        Target::put_call(code, runtime::i64load);
    }
    static constexpr void f32load(uint8_t *&code) {
        Target::put_call(code, runtime::f32load);
    }
    static constexpr void f64load(uint8_t *&code) {
        Target::put_call(code, runtime::f64load);
    }
    static constexpr void i32load8_s(uint8_t *&code) {
        Target::put_call(code, runtime::i32load8_s);
    }
    static constexpr void i32load8_u(uint8_t *&code) {
        Target::put_call(code, runtime::i32load8_u);
    }
    static constexpr void i32load16_s(uint8_t *&code) {
        Target::put_call(code, runtime::i32load16_s);
    }
    static constexpr void i32load16_u(uint8_t *&code) {
        Target::put_call(code, runtime::i32load16_u);
    }
    static constexpr void i64load8_s(uint8_t *&code) {
        Target::put_call(code, runtime::i64load8_s);
    }
    static constexpr void i64load8_u(uint8_t *&code) {
        Target::put_call(code, runtime::i64load8_u);
    }
    static constexpr void i64load16_s(uint8_t *&code) {
        Target::put_call(code, runtime::i64load16_s);
    }
    static constexpr void i64load16_u(uint8_t *&code) {
        Target::put_call(code, runtime::i64load16_u);
    }
    static constexpr void i64load32_s(uint8_t *&code) {
        Target::put_call(code, runtime::i64load32_s);
    }
    static constexpr void i64load32_u(uint8_t *&code) {
        Target::put_call(code, runtime::i64load32_u);
    }
    static constexpr void i32store(uint8_t *&code) {
        Target::put_call(code, runtime::i32store);
    }
    static constexpr void i64store(uint8_t *&code) {
        Target::put_call(code, runtime::i64store);
    }
    static constexpr void f32store(uint8_t *&code) {
        Target::put_call(code, runtime::f32store);
    }
    static constexpr void f64store(uint8_t *&code) {
        Target::put_call(code, runtime::f64store);
    }
    static constexpr void i32store8(uint8_t *&code) {
        Target::put_call(code, runtime::i32store8);
    }
    static constexpr void i32store16(uint8_t *&code) {
        Target::put_call(code, runtime::i32store16);
    }
    static constexpr void i64store8(uint8_t *&code) {
        Target::put_call(code, runtime::i64store8);
    }
    static constexpr void i64store16(uint8_t *&code) {
        Target::put_call(code, runtime::i64store16);
    }
    static constexpr void i64store32(uint8_t *&code) {
        Target::put_call(code, runtime::i64store32);
    }
    static constexpr void i32eqz(uint8_t *&code) {
        Target::put_call(code, runtime::i32eqz);
    }
    static constexpr void i64eqz(uint8_t *&code) {
        Target::put_call(code, runtime::i64eqz);
    }
    static constexpr void i32eq(uint8_t *&code) {
        Target::put_call(code, runtime::i32eq);
    }
    static constexpr void i64eq(uint8_t *&code) {
        Target::put_call(code, runtime::i64eq);
    }
    static constexpr void i32ne(uint8_t *&code) {
        Target::put_call(code, runtime::i32ne);
    }
    static constexpr void i64ne(uint8_t *&code) {
        Target::put_call(code, runtime::i64ne);
    }
    static constexpr void i32lt_s(uint8_t *&code) {
        Target::put_call(code, runtime::i32lt_s);
    }
    static constexpr void i64lt_s(uint8_t *&code) {
        Target::put_call(code, runtime::i64lt_s);
    }
    static constexpr void i32lt_u(uint8_t *&code) {
        Target::put_call(code, runtime::i32lt_u);
    }
    static constexpr void i64lt_u(uint8_t *&code) {
        Target::put_call(code, runtime::i64lt_u);
    }
    static constexpr void i32gt_s(uint8_t *&code) {
        Target::put_call(code, runtime::i32gt_s);
    }
    static constexpr void i64gt_s(uint8_t *&code) {
        Target::put_call(code, runtime::i64gt_s);
    }
    static constexpr void i32gt_u(uint8_t *&code) {
        Target::put_call(code, runtime::i32gt_u);
    }
    static constexpr void i64gt_u(uint8_t *&code) {
        Target::put_call(code, runtime::i64gt_u);
    }
    static constexpr void i32le_s(uint8_t *&code) {
        Target::put_call(code, runtime::i32le_s);
    }
    static constexpr void i64le_s(uint8_t *&code) {
        Target::put_call(code, runtime::i64le_s);
    }
    static constexpr void i32le_u(uint8_t *&code) {
        Target::put_call(code, runtime::i32le_u);
    }
    static constexpr void i64le_u(uint8_t *&code) {
        Target::put_call(code, runtime::i64le_u);
    }
    static constexpr void i32ge_s(uint8_t *&code) {
        Target::put_call(code, runtime::i32ge_s);
    }
    static constexpr void i64ge_s(uint8_t *&code) {
        Target::put_call(code, runtime::i64ge_s);
    }
    static constexpr void i32ge_u(uint8_t *&code) {
        Target::put_call(code, runtime::i32ge_u);
    }
    static constexpr void i64ge_u(uint8_t *&code) {
        Target::put_call(code, runtime::i64ge_u);
    }
    static constexpr void f32eq(uint8_t *&code) {
        Target::put_call(code, runtime::f32eq);
    }
    static constexpr void f64eq(uint8_t *&code) {
        Target::put_call(code, runtime::f64eq);
    }
    static constexpr void f32ne(uint8_t *&code) {
        Target::put_call(code, runtime::f32ne);
    }
    static constexpr void f64ne(uint8_t *&code) {
        Target::put_call(code, runtime::f64ne);
    }
    static constexpr void f32lt(uint8_t *&code) {
        Target::put_call(code, runtime::f32lt);
    }
    static constexpr void f64lt(uint8_t *&code) {
        Target::put_call(code, runtime::f64lt);
    }
    static constexpr void f32gt(uint8_t *&code) {
        Target::put_call(code, runtime::f32gt);
    }
    static constexpr void f64gt(uint8_t *&code) {
        Target::put_call(code, runtime::f64gt);
    }
    static constexpr void f32le(uint8_t *&code) {
        Target::put_call(code, runtime::f32le);
    }
    static constexpr void f64le(uint8_t *&code) {
        Target::put_call(code, runtime::f64le);
    }
    static constexpr void f32ge(uint8_t *&code) {
        Target::put_call(code, runtime::f32ge);
    }
    static constexpr void f64ge(uint8_t *&code) {
        Target::put_call(code, runtime::f64ge);
    }
    static constexpr void i32clz(uint8_t *&code) {
        Target::put_call(code, runtime::i32clz);
    }
    static constexpr void i64clz(uint8_t *&code) {
        Target::put_call(code, runtime::i64clz);
    }
    static constexpr void i32ctz(uint8_t *&code) {
        Target::put_call(code, runtime::i32ctz);
    }
    static constexpr void i64ctz(uint8_t *&code) {
        Target::put_call(code, runtime::i64ctz);
    }
    static constexpr void i32popcnt(uint8_t *&code) {
        Target::put_call(code, runtime::i32popcnt);
    }
    static constexpr void i64popcnt(uint8_t *&code) {
        Target::put_call(code, runtime::i64popcnt);
    }
    static constexpr void i32add(uint8_t *&code) {
        Target::put_call(code, runtime::i32add);
    }
    static constexpr void i64add(uint8_t *&code) {
        Target::put_call(code, runtime::i64add);
    }
    static constexpr void i32sub(uint8_t *&code) {
        Target::put_call(code, runtime::i32sub);
    }
    static constexpr void i64sub(uint8_t *&code) {
        Target::put_call(code, runtime::i64sub);
    }
    static constexpr void i32mul(uint8_t *&code) {
        Target::put_call(code, runtime::i32mul);
    }
    static constexpr void i64mul(uint8_t *&code) {
        Target::put_call(code, runtime::i64mul);
    }
    static constexpr void i32div_s(uint8_t *&code) {
        Target::put_call(code, runtime::i32div_s);
    }
    static constexpr void i64div_s(uint8_t *&code) {
        Target::put_call(code, runtime::i64div_s);
    }
    static constexpr void i32div_u(uint8_t *&code) {
        Target::put_call(code, runtime::i32div_u);
    }
    static constexpr void i64div_u(uint8_t *&code) {
        Target::put_call(code, runtime::i64div_u);
    }
    static constexpr void i32rem_s(uint8_t *&code) {
        Target::put_call(code, runtime::i32rem_s);
    }
    static constexpr void i64rem_s(uint8_t *&code) {
        Target::put_call(code, runtime::i64rem_s);
    }
    static constexpr void i32rem_u(uint8_t *&code) {
        Target::put_call(code, runtime::i32rem_u);
    }
    static constexpr void i64rem_u(uint8_t *&code) {
        Target::put_call(code, runtime::i64rem_u);
    }
    static constexpr void i32and(uint8_t *&code) {
        Target::put_call(code, runtime::i32and);
    }
    static constexpr void i64and(uint8_t *&code) {
        Target::put_call(code, runtime::i64and);
    }
    static constexpr void i32or(uint8_t *&code) {
        Target::put_call(code, runtime::i32or);
    }
    static constexpr void i64or(uint8_t *&code) {
        Target::put_call(code, runtime::i64or);
    }
    static constexpr void i32xor(uint8_t *&code) {
        Target::put_call(code, runtime::i32xor);
    }
    static constexpr void i64xor(uint8_t *&code) {
        Target::put_call(code, runtime::i64xor);
    }
    static constexpr void i32shl(uint8_t *&code) {
        Target::put_call(code, runtime::i32shl);
    }
    static constexpr void i64shl(uint8_t *&code) {
        Target::put_call(code, runtime::i64shl);
    }
    static constexpr void i32shr_s(uint8_t *&code) {
        Target::put_call(code, runtime::i32shr_s);
    }
    static constexpr void i64shr_s(uint8_t *&code) {
        Target::put_call(code, runtime::i64shr_s);
    }
    static constexpr void i32shr_u(uint8_t *&code) {
        Target::put_call(code, runtime::i32shr_u);
    }
    static constexpr void i64shr_u(uint8_t *&code) {
        Target::put_call(code, runtime::i64shr_u);
    }
    static constexpr void i32rotl(uint8_t *&code) {
        Target::put_call(code, runtime::i32rotl);
    }
    static constexpr void i64rotl(uint8_t *&code) {
        Target::put_call(code, runtime::i64rotl);
    }
    static constexpr void i32rotr(uint8_t *&code) {
        Target::put_call(code, runtime::i32rotr);
    }
    static constexpr void i64rotr(uint8_t *&code) {
        Target::put_call(code, runtime::i64rotr);
    }
    static constexpr void f32abs(uint8_t *&code) {
        Target::put_call(code, runtime::f32abs);
    }
    static constexpr void f64abs(uint8_t *&code) {
        Target::put_call(code, runtime::f64abs);
    }
    static constexpr void f32neg(uint8_t *&code) {
        Target::put_call(code, runtime::f32neg);
    }
    static constexpr void f64neg(uint8_t *&code) {
        Target::put_call(code, runtime::f64neg);
    }
    static constexpr void f32ceil(uint8_t *&code) {
        Target::put_call(code, runtime::f32ceil);
    }
    static constexpr void f64ceil(uint8_t *&code) {
        Target::put_call(code, runtime::f64ceil);
    }
    static constexpr void f32floor(uint8_t *&code) {
        Target::put_call(code, runtime::f32floor);
    }
    static constexpr void f64floor(uint8_t *&code) {
        Target::put_call(code, runtime::f64floor);
    }
    static constexpr void f32trunc(uint8_t *&code) {
        Target::put_call(code, runtime::f32trunc);
    }
    static constexpr void f64trunc(uint8_t *&code) {
        Target::put_call(code, runtime::f64trunc);
    }
    static constexpr void f32nearest(uint8_t *&code) {
        Target::put_call(code, runtime::f32nearest);
    }
    static constexpr void f64nearest(uint8_t *&code) {
        Target::put_call(code, runtime::f64nearest);
    }
    static constexpr void f32sqrt(uint8_t *&code) {
        Target::put_call(code, runtime::f32sqrt);
    }
    static constexpr void f64sqrt(uint8_t *&code) {
        Target::put_call(code, runtime::f64sqrt);
    }
    static constexpr void f32add(uint8_t *&code) {
        Target::put_call(code, runtime::f32add);
    }
    static constexpr void f64add(uint8_t *&code) {
        Target::put_call(code, runtime::f64add);
    }
    static constexpr void f32sub(uint8_t *&code) {
        Target::put_call(code, runtime::f32sub);
    }
    static constexpr void f64sub(uint8_t *&code) {
        Target::put_call(code, runtime::f64sub);
    }
    static constexpr void f32mul(uint8_t *&code) {
        Target::put_call(code, runtime::f32mul);
    }
    static constexpr void f64mul(uint8_t *&code) {
        Target::put_call(code, runtime::f64mul);
    }
    static constexpr void f32div(uint8_t *&code) {
        Target::put_call(code, runtime::f32div);
    }
    static constexpr void f64div(uint8_t *&code) {
        Target::put_call(code, runtime::f64div);
    }
    static constexpr void f32min(uint8_t *&code) {
        Target::put_call(code, runtime::f32min);
    }
    static constexpr void f64min(uint8_t *&code) {
        Target::put_call(code, runtime::f64min);
    }
    static constexpr void f32max(uint8_t *&code) {
        Target::put_call(code, runtime::f32max);
    }
    static constexpr void f64max(uint8_t *&code) {
        Target::put_call(code, runtime::f64max);
    }
    static constexpr void f32copysign(uint8_t *&code) {
        Target::put_call(code, runtime::f32copysign);
    }
    static constexpr void f64copysign(uint8_t *&code) {
        Target::put_call(code, runtime::f64copysign);
    }
    static constexpr void i32wrap_i64(uint8_t *&code) {
        Target::put_call(code, runtime::i32wrap_i64);
    }
    static constexpr void i64extend_i32_s(uint8_t *&code) {
        Target::put_call(code, runtime::i64extend_i32_s);
    }
    static constexpr void i64extend_i32_u(uint8_t *&code) {
        Target::put_call(code, runtime::i64extend_i32_u);
    }
    static constexpr void i32trunc_f32_s(uint8_t *&code) {
        Target::put_call(code, runtime::i32trunc_f32_s);
    }
    static constexpr void i64trunc_f32_s(uint8_t *&code) {
        Target::put_call(code, runtime::i64trunc_f32_s);
    }
    static constexpr void i32trunc_f32_u(uint8_t *&code) {
        Target::put_call(code, runtime::i32trunc_f32_u);
    }
    static constexpr void i64trunc_f32_u(uint8_t *&code) {
        Target::put_call(code, runtime::i64trunc_f32_u);
    }
    static constexpr void i32trunc_f64_s(uint8_t *&code) {
        Target::put_call(code, runtime::i32trunc_f64_s);
    }
    static constexpr void i64trunc_f64_s(uint8_t *&code) {
        Target::put_call(code, runtime::i64trunc_f64_s);
    }
    static constexpr void i32trunc_f64_u(uint8_t *&code) {
        Target::put_call(code, runtime::i32trunc_f64_u);
    }
    static constexpr void i64trunc_f64_u(uint8_t *&code) {
        Target::put_call(code, runtime::i64trunc_f64_u);
    }
    static constexpr void f32convert_i32_s(uint8_t *&code) {
        Target::put_call(code, runtime::f32convert_i32_s);
    }
    static constexpr void f64convert_i32_s(uint8_t *&code) {
        Target::put_call(code, runtime::f64convert_i32_s);
    }
    static constexpr void f32convert_i32_u(uint8_t *&code) {
        Target::put_call(code, runtime::f32convert_i32_u);
    }
    static constexpr void f64convert_i32_u(uint8_t *&code) {
        Target::put_call(code, runtime::f64convert_i32_u);
    }
    static constexpr void f32convert_i64_s(uint8_t *&code) {
        Target::put_call(code, runtime::f32convert_i64_s);
    }
    static constexpr void f64convert_i64_s(uint8_t *&code) {
        Target::put_call(code, runtime::f64convert_i64_s);
    }
    static constexpr void f32convert_i64_u(uint8_t *&code) {
        Target::put_call(code, runtime::f32convert_i64_u);
    }
    static constexpr void f64convert_i64_u(uint8_t *&code) {
        Target::put_call(code, runtime::f64convert_i64_u);
    }
    static constexpr void f32demote_f64(uint8_t *&code) {
        Target::put_call(code, runtime::f32demote_f64);
    }
    static constexpr void f64promote_f32(uint8_t *&code) {
        Target::put_call(code, runtime::f64promote_f32);
    }
    static constexpr void i32reinterpret_f32(uint8_t *&code) {
        Target::put_call(code, runtime::i32reinterpret_f32);
    }
    static constexpr void f32reinterpret_i32(uint8_t *&code) {
        Target::put_call(code, runtime::f32reinterpret_i32);
    }
    static constexpr void i64reinterpret_f64(uint8_t *&code) {
        Target::put_call(code, runtime::i64reinterpret_f64);
    }
    static constexpr void f64reinterpret_i64(uint8_t *&code) {
        Target::put_call(code, runtime::f64reinterpret_i64);
    }
    static constexpr void i32extend8_s(uint8_t *&code) {
        Target::put_call(code, runtime::i32extend8_s);
    }
    static constexpr void i32extend16_s(uint8_t *&code) {
        Target::put_call(code, runtime::i32extend16_s);
    }
    static constexpr void i64extend8_s(uint8_t *&code) {
        Target::put_call(code, runtime::i64extend8_s);
    }
    static constexpr void i64extend16_s(uint8_t *&code) {
        Target::put_call(code, runtime::i64extend16_s);
    }
    static constexpr void i64extend32_s(uint8_t *&code) {
        Target::put_call(code, runtime::i64extend32_s);
    }
    static constexpr void ref_null(uint8_t *&code) {
        Target::put_call(code, runtime::ref_null);
    }
    static constexpr void ref_is_null(uint8_t *&code) {
        Target::put_call(code, runtime::ref_is_null);
    }
    static constexpr void ref_func(uint8_t *&code) {
        Target::put_call(code, runtime::ref_func);
    }
    static constexpr void ref_eq(uint8_t *&code) {
        Target::put_call(code, runtime::ref_eq);
    }
    static constexpr void i32_trunc_sat_f32_s(uint8_t *&code) {
        Target::put_call(code, runtime::i32_trunc_sat_f32_s);
    }
    static constexpr void i32_trunc_sat_f32_u(uint8_t *&code) {
        Target::put_call(code, runtime::i32_trunc_sat_f32_u);
    }
    static constexpr void i32_trunc_sat_f64_s(uint8_t *&code) {
        Target::put_call(code, runtime::i32_trunc_sat_f64_s);
    }
    static constexpr void i32_trunc_sat_f64_u(uint8_t *&code) {
        Target::put_call(code, runtime::i32_trunc_sat_f64_u);
    }
    static constexpr void i64_trunc_sat_f32_s(uint8_t *&code) {
        Target::put_call(code, runtime::i64_trunc_sat_f32_s);
    }
    static constexpr void i64_trunc_sat_f32_u(uint8_t *&code) {
        Target::put_call(code, runtime::i64_trunc_sat_f32_u);
    }
    static constexpr void i64_trunc_sat_f64_s(uint8_t *&code) {
        Target::put_call(code, runtime::i64_trunc_sat_f64_s);
    }
    static constexpr void i64_trunc_sat_f64_u(uint8_t *&code) {
        Target::put_call(code, runtime::i64_trunc_sat_f64_u);
    }
    static constexpr void memory_init(uint8_t *&code) {
        Target::put_call(code, runtime::memory_init);
    }
    static constexpr void data_drop(uint8_t *&code) {
        Target::put_call(code, runtime::data_drop);
    }
    static constexpr void memory_copy(uint8_t *&code) {
        Target::put_call(code, runtime::memory_copy);
    }
    static constexpr void memory_fill(uint8_t *&code) {
        Target::put_call(code, runtime::memory_fill);
    }
    static constexpr void table_init(uint8_t *&code) {
        Target::put_call(code, runtime::table_init);
    }
    static constexpr void elem_drop(uint8_t *&code) {
        Target::put_call(code, runtime::elem_drop);
    }
    static constexpr void table_copy(uint8_t *&code) {
        Target::put_call(code, runtime::table_copy);
    }
    static constexpr void table_grow(uint8_t *&code) {
        Target::put_call(code, runtime::table_grow);
    }
    static constexpr void table_size(uint8_t *&code) {
        Target::put_call(code, runtime::table_size);
    }
    static constexpr void table_fill(uint8_t *&code) {
        Target::put_call(code, runtime::table_fill);
    }
    static constexpr void multibyte(uint8_t *&code) {
        Target::put_call(code, runtime::multibyte);
    }
};

} // namespace mitey