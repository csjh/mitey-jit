#include "runtime.hpp"
#include "instance.hpp"
#include <algorithm>
#include <bit>
#include <cmath>

namespace mitey {
namespace runtime {

#define HANDLER(name)                                                          \
    void name(WasmMemory *memory, WasmValue *stack, void **misc,               \
              uint64_t tmp1, uint64_t tmp2)
#define PARAMS memory, stack, misc, tmp1, tmp2
#define PRELUDE                                                                \
    do {                                                                       \
    } while (0)
#define POSTLUDE return dummy(memory, stack, misc)
#define MISC_GET(type, idx) (*reinterpret_cast<type *>(misc[idx]))

__attribute__((noinline)) void dummy(WasmMemory *memory, WasmValue *stack,
                                     void **misc) {
    asm volatile("" ::"r"(memory), "r"(stack), "r"(misc));
    return;
}

HANDLER(unreachable) { trap("unreachable"); }
HANDLER(if_) {
    // tmp1 = else branch
    PRELUDE;
    --stack;
    if (!stack->u32) {
        [[clang::musttail]] return reinterpret_cast<Signature *>(tmp1)(PARAMS);
    }
    POSTLUDE;
}
HANDLER(br) {
    // tmp1 = target
    // tmp2 = brinfo
    PRELUDE;
    auto info = std::bit_cast<BrInfo>(tmp2);
    auto new_stack = stack - info.stack_offset;
    std::memmove(new_stack - info.arity, stack - info.arity,
                 info.arity * sizeof(WasmValue));
    stack = new_stack;
    [[clang::musttail]] return reinterpret_cast<Signature *>(tmp1)(PARAMS);
    POSTLUDE;
}
// todo:
// HANDLER(br_0);
// HANDLER(br_4);
// HANDLER(br_8);
// and if/table equivalents

HANDLER(br_if) {
    // tmp1 = target
    // tmp2 = brinfo
    PRELUDE;
    --stack;
    if (stack->u32) {
        [[clang::musttail]] return br(PARAMS);
    }
    POSTLUDE;
}
HANDLER(br_table) {
    // tmp1 = lookup table addr
    // tmp2 = brinfo
    PRELUDE;
    auto lookup = reinterpret_cast<BrTableTarget *>(tmp1);
    auto info = std::bit_cast<BrInfo>(tmp2);
    --stack;
    auto jump = std::max(static_cast<uint32_t>(info.n_targets), stack->u32);
    auto target = lookup[jump];
    info.stack_offset = target.stack_offset;
    tmp1 /* dest */ = reinterpret_cast<uint64_t>(lookup + target.lookup_offset);
    tmp2 = std::bit_cast<uint64_t>(info);
    [[clang::musttail]] return br(PARAMS);
    POSTLUDE;
}
HANDLER(call) {
    // tmp1 = function start
    PRELUDE;
    reinterpret_cast<Signature *>(tmp1)(PARAMS);
    POSTLUDE;
}
HANDLER(call_extern) {
    // tmp1 = function offset in misc
    PRELUDE;
    MISC_GET(Signature *, tmp1)(PARAMS);
    POSTLUDE;
}
HANDLER(call_indirect) {
    PRELUDE;

    uint64_t combined[] = {tmp1, tmp2};
    auto info = std::bit_cast<CallIndirectInfo>(combined);
    auto &table = MISC_GET(WasmTable, info.table_idx);

    --stack;
    auto elem_idx = stack->u32;

    if (elem_idx >= table.size()) {
        trap("undefined element");
    }
    auto funcref = table.get(elem_idx).funcref;
    if (!funcref) {
        trap("uninitialized element");
    }
    if (funcref->type != info.type) {
        trap("indirect call type mismatch");
    }

    auto func = funcref->signature;
    auto &instance = *funcref->instance.get();
    func(instance.memory.get(), stack, instance.misc.get(), tmp1, tmp2);

    POSTLUDE;
}
HANDLER(drop) {
    PRELUDE;
    --stack;
    POSTLUDE;
}
HANDLER(select) {
    PRELUDE;
    stack -= 2;
    stack[-1] = stack[-!!stack[1].u32];
    POSTLUDE;
}
HANDLER(select_t) {
    PRELUDE;
    stack -= 2;
    stack[-1] = stack[-!!stack[1].u32];
    POSTLUDE;
}
HANDLER(localget) {
    // tmp1 = local index/stack offset to local
    PRELUDE;
    auto &local = stack[tmp1];
    *stack++ = local;
    POSTLUDE;
}
HANDLER(localset) {
    // tmp1 = local index/stack offset to local
    PRELUDE;
    auto &local = stack[tmp1];
    local = *--stack;
    POSTLUDE;
}
HANDLER(localtee) {
    // tmp1 = local index/stack offset to local
    PRELUDE;
    stack[tmp1] = stack[-1];
    POSTLUDE;
}
HANDLER(tableget) {
    // tmp1 = table index in misc table
    PRELUDE;
    auto &table = MISC_GET(WasmTable, tmp1);
    auto idx = stack[-1].u32;
    stack[-1] = table.get(idx);
    POSTLUDE;
}
HANDLER(tableset) {
    // tmp1 = table index in misc table
    PRELUDE;
    auto &table = MISC_GET(WasmTable, tmp1);
    stack -= 2;
    auto idx = stack[0].u32;
    auto val = stack[1];
    table.set(idx, val);
    POSTLUDE;
}
HANDLER(globalget) {
    // tmp1 = global index in misc table
    PRELUDE;
    auto global = MISC_GET(WasmValue *, tmp1);
    *stack++ = *global;
    POSTLUDE;
}
HANDLER(globalset) {
    // tmp1 = global index in misc table
    PRELUDE;
    auto global = MISC_GET(WasmValue *, tmp1);
    *global = *--stack;
    POSTLUDE;
}
HANDLER(memorysize) {
    PRELUDE;
    *stack++ = memory->size();
    POSTLUDE;
}
HANDLER(memorygrow) {
    PRELUDE;
    stack[-1].u32 = memory->grow(stack[-1].u32);
    POSTLUDE;
}
HANDLER(ifXXconst) {
    // tmp1 = 64 bits of literal
    PRELUDE;
    *stack++ = tmp1;
    POSTLUDE;
}

using u32 = uint32_t;
using u64 = uint64_t;
using i32 = int32_t;
using i64 = int64_t;
using f32 = float;
using f64 = double;

#define UNARY_OP(type, op)                                                     \
    PRELUDE;                                                                   \
    stack[-1] = op(stack[-1].type);                                            \
    POSTLUDE;
#define TRUNC(type, op, lower, upper)                                          \
    {                                                                          \
        PRELUDE;                                                               \
        if (!std::isfinite(stack[-1].type)) {                                  \
            if (std::isnan(stack[-1].type)) {                                  \
                trap("invalid conversion to integer");                         \
            } else {                                                           \
                trap("integer overflow");                                      \
            }                                                                  \
        }                                                                      \
        if (stack[-1].type <= lower || upper <= stack[-1].type) {              \
            trap("integer overflow");                                          \
        }                                                                      \
        stack[-1] = op(stack[-1].type);                                        \
        POSTLUDE;                                                              \
    }
#define UNARY_FN(type, fn)                                                     \
    PRELUDE;                                                                   \
    stack[-1] = fn(stack[-1].type);                                            \
    POSTLUDE
#define BINARY_OP(type, op)                                                    \
    {                                                                          \
        PRELUDE;                                                               \
        stack--;                                                               \
        stack[-1] = stack[-1].type op stack[0].type;                           \
        POSTLUDE;                                                              \
    }
#define BINARY_FN(type, fn)                                                    \
    {                                                                          \
        PRELUDE;                                                               \
        stack--;                                                               \
        stack[-1] = fn(stack[-1].type, stack[0].type);                         \
        POSTLUDE;                                                              \
    }
#define IDIV(type)                                                             \
    {                                                                          \
        PRELUDE;                                                               \
        stack--;                                                               \
        if (stack[0].type == 0) {                                              \
            trap("integer divide by zero");                                    \
        }                                                                      \
        if (std::is_signed_v<type> &&                                          \
            stack[0].type == static_cast<type>(-1) &&                          \
            stack[-1].type == std::numeric_limits<type>::min()) {              \
            trap("integer overflow");                                          \
        }                                                                      \
        stack[-1] = stack[-1].type / stack[0].type;                            \
        POSTLUDE;                                                              \
    }
#define IREM(type)                                                             \
    {                                                                          \
        PRELUDE;                                                               \
        stack--;                                                               \
        if (stack[0].type == 0) {                                              \
            trap("integer divide by zero");                                    \
        }                                                                      \
        if (std::is_signed_v<type> &&                                          \
            stack[0].type == static_cast<type>(-1) &&                          \
            stack[-1].type == std::numeric_limits<type>::min()) [[unlikely]] { \
            stack[-1] = static_cast<type>(0);                                  \
        } else {                                                               \
            stack[-1] = stack[-1].type % stack[0].type;                        \
        }                                                                      \
        POSTLUDE;                                                              \
    }
#define MINMAX(type, fn)                                                       \
    {                                                                          \
        PRELUDE;                                                               \
        stack--;                                                               \
        if (std::isnan(stack[-1].type) || std::isnan(stack[0].type)) {         \
            stack[-1].type = std::numeric_limits<type>::quiet_NaN();           \
        } else {                                                               \
            stack[-1].type = fn(stack[-1].type, stack[0].type);                \
        }                                                                      \
        POSTLUDE;                                                              \
    }
#define SHIFT(type, op)                                                        \
    {                                                                          \
        PRELUDE;                                                               \
        stack--;                                                               \
        stack[-1] = stack[-1].type op(stack[0].type % (sizeof(type) * 8));     \
        POSTLUDE;                                                              \
    }
#define TRUNC_SAT(from, to)                                                    \
    {                                                                          \
        PRELUDE;                                                               \
        if (stack[-1].from <                                                   \
            static_cast<from>(std::numeric_limits<to>::min())) {               \
            stack[-1].to = std::numeric_limits<to>::min();                     \
        } else if (stack[-1].from >                                            \
                   static_cast<from>(std::numeric_limits<to>::max())) {        \
            stack[-1].to = std::numeric_limits<to>::max();                     \
        } else {                                                               \
            stack[-1].to = static_cast<to>(stack[-1].from);                    \
        }                                                                      \
        POSTLUDE;                                                              \
    }

#define LOAD(stacktype, memtype)                                               \
    {                                                                          \
        /* tmp1 = offset */                                                    \
        PRELUDE;                                                               \
        stack[-1] = memory->load<stacktype, memtype>(stack[-1].u32 + tmp1);    \
        POSTLUDE;                                                              \
    }

#define STORE(stacktype, memtype)                                              \
    {                                                                          \
        /* tmp1 = offset */                                                    \
        PRELUDE;                                                               \
        stack -= 2;                                                            \
        memory->store<stacktype, memtype>(stack[0].u32 + tmp1, stack[1]);      \
        POSTLUDE;                                                              \
    }

// clang-format off
HANDLER(i32load)      { LOAD(u32, uint32_t); }
HANDLER(i64load)      { LOAD(u64, uint64_t); }
HANDLER(f32load)      { LOAD(f32, float); }
HANDLER(f64load)      { LOAD(f64, double); }
HANDLER(i32load8_s)   { LOAD(i32, int8_t); }
HANDLER(i32load8_u)   { LOAD(u32, uint8_t); }
HANDLER(i32load16_s)  { LOAD(i32, int16_t); }
HANDLER(i32load16_u)  { LOAD(u32, uint16_t); }
HANDLER(i64load8_s)   { LOAD(i64, int8_t); }
HANDLER(i64load8_u)   { LOAD(u64, uint8_t); }
HANDLER(i64load16_s)  { LOAD(i64, int16_t); }
HANDLER(i64load16_u)  { LOAD(u64, uint16_t); }
HANDLER(i64load32_s)  { LOAD(i64, int32_t); }
HANDLER(i64load32_u)  { LOAD(u64, uint32_t); }
HANDLER(i32store)     { STORE(u32, uint32_t); }
HANDLER(i64store)     { STORE(u64, uint64_t); }
HANDLER(f32store)     { STORE(f32, float); }
HANDLER(f64store)     { STORE(f64, double); }
HANDLER(i32store8)    { STORE(u32, uint8_t); }
HANDLER(i32store16)   { STORE(u32, uint16_t); }
HANDLER(i64store8)    { STORE(u64, uint8_t); }
HANDLER(i64store16)   { STORE(u64, uint16_t); }
HANDLER(i64store32)   { STORE(u64, uint32_t); }
HANDLER(i32eqz)       { UNARY_OP (i32, 0 ==); }
HANDLER(i64eqz)       { UNARY_OP (i64, 0 ==); }
HANDLER(i32eq)        { BINARY_OP(i32, ==); }
HANDLER(i64eq)        { BINARY_OP(i64, ==); }
HANDLER(i32ne)        { BINARY_OP(i32, !=); }
HANDLER(i64ne)        { BINARY_OP(i64, !=); }
HANDLER(i32lt_s)      { BINARY_OP(i32, < ); }
HANDLER(i64lt_s)      { BINARY_OP(i64, < ); }
HANDLER(i32lt_u)      { BINARY_OP(u32, < ); }
HANDLER(i64lt_u)      { BINARY_OP(u64, < ); }
HANDLER(i32gt_s)      { BINARY_OP(i32, > ); }
HANDLER(i64gt_s)      { BINARY_OP(i64, > ); }
HANDLER(i32gt_u)      { BINARY_OP(u32, > ); }
HANDLER(i64gt_u)      { BINARY_OP(u64, > ); }
HANDLER(i32le_s)      { BINARY_OP(i32, <=); }
HANDLER(i64le_s)      { BINARY_OP(i64, <=); }
HANDLER(i32le_u)      { BINARY_OP(u32, <=); }
HANDLER(i64le_u)      { BINARY_OP(u64, <=); }
HANDLER(i32ge_s)      { BINARY_OP(i32, >=); }
HANDLER(i64ge_s)      { BINARY_OP(i64, >=); }
HANDLER(i32ge_u)      { BINARY_OP(u32, >=); }
HANDLER(i64ge_u)      { BINARY_OP(u64, >=); }
HANDLER(f32eq)        { BINARY_OP(f32, ==); }
HANDLER(f64eq)        { BINARY_OP(f64, ==); }
HANDLER(f32ne)        { BINARY_OP(f32, !=); }
HANDLER(f64ne)        { BINARY_OP(f64, !=); }
HANDLER(f32lt)        { BINARY_OP(f32, < ); }
HANDLER(f64lt)        { BINARY_OP(f64, < ); }
HANDLER(f32gt)        { BINARY_OP(f32, > ); }
HANDLER(f64gt)        { BINARY_OP(f64, > ); }
HANDLER(f32le)        { BINARY_OP(f32, <=); }
HANDLER(f64le)        { BINARY_OP(f64, <=); }
HANDLER(f32ge)        { BINARY_OP(f32, >=); }
HANDLER(f64ge)        { BINARY_OP(f64, >=); }
HANDLER(i32clz)       { UNARY_FN (u32, std::countl_zero); }
HANDLER(i64clz)       { UNARY_FN (u64, (uint64_t)std::countl_zero); }
HANDLER(i32ctz)       { UNARY_FN (u32, std::countr_zero); }
HANDLER(i64ctz)       { UNARY_FN (u64, (uint64_t)std::countr_zero); }
HANDLER(i32popcnt)    { UNARY_FN (u32, std::popcount); }
HANDLER(i64popcnt)    { UNARY_FN (u64, (uint64_t)std::popcount); }
HANDLER(i32add)       { BINARY_OP(u32, + ); }
HANDLER(i64add)       { BINARY_OP(u64, + ); }
HANDLER(i32sub)       { BINARY_OP(u32, - ); }
HANDLER(i64sub)       { BINARY_OP(u64, - ); }
HANDLER(i32mul)       { BINARY_OP(u32, * ); }
HANDLER(i64mul)       { BINARY_OP(u64, * ); }
HANDLER(i32div_s)     { IDIV     (i32); }
HANDLER(i64div_s)     { IDIV     (i64); }
HANDLER(i32div_u)     { IDIV     (u32); }
HANDLER(i64div_u)     { IDIV     (u64); }
HANDLER(i32rem_s)     { IREM     (i32); }
HANDLER(i64rem_s)     { IREM     (i64); }
HANDLER(i32rem_u)     { IREM     (u32); }
HANDLER(i64rem_u)     { IREM     (u64); }
HANDLER(i32and)       { BINARY_OP(u32, & ); }
HANDLER(i64and)       { BINARY_OP(u64, & ); }
HANDLER(i32or)        { BINARY_OP(u32, | ); }
HANDLER(i64or)        { BINARY_OP(u64, | ); }
HANDLER(i32xor)       { BINARY_OP(u32, ^ ); }
HANDLER(i64xor)       { BINARY_OP(u64, ^ ); }
HANDLER(i32shl)       { SHIFT    (u32, <<); }
HANDLER(i64shl)       { SHIFT    (u64, <<); }
HANDLER(i32shr_s)     { SHIFT    (i32, >>); }
HANDLER(i64shr_s)     { SHIFT    (i64, >>); }
HANDLER(i32shr_u)     { SHIFT    (u32, >>); }
HANDLER(i64shr_u)     { SHIFT    (u64, >>); }
HANDLER(i32rotl)      { BINARY_FN(u32, std::rotl); }
HANDLER(i64rotl)      { BINARY_FN(u64, std::rotl); }
HANDLER(i32rotr)      { BINARY_FN(u32, std::rotr); }
HANDLER(i64rotr)      { BINARY_FN(u64, std::rotr); }
HANDLER(f32abs)       { UNARY_FN (f32, std::abs); }
HANDLER(f64abs)       { UNARY_FN (f64, std::abs); }
HANDLER(f32neg)       { UNARY_OP (f32, -); }
HANDLER(f64neg)       { UNARY_OP (f64, -); }
HANDLER(f32ceil)      { UNARY_FN (f32, std::ceil); }
HANDLER(f64ceil)      { UNARY_FN (f64, std::ceil); }
HANDLER(f32floor)     { UNARY_FN (f32, std::floor); }
HANDLER(f64floor)     { UNARY_FN (f64, std::floor); }
HANDLER(f32trunc)     { UNARY_FN (f32, std::trunc); }
HANDLER(f64trunc)     { UNARY_FN (f64, std::trunc); }
HANDLER(f32nearest)   { UNARY_FN (f32, std::nearbyint); }
HANDLER(f64nearest)   { UNARY_FN (f64, std::nearbyint); }
HANDLER(f32sqrt)      { UNARY_FN (f32, std::sqrt); }
HANDLER(f64sqrt)      { UNARY_FN (f64, std::sqrt); }
HANDLER(f32add)       { BINARY_OP(f32, +); }
HANDLER(f64add)       { BINARY_OP(f64, +); }
HANDLER(f32sub)       { BINARY_OP(f32, -); }
HANDLER(f64sub)       { BINARY_OP(f64, -); }
HANDLER(f32mul)       { BINARY_OP(f32, *); }
HANDLER(f64mul)       { BINARY_OP(f64, *); }
HANDLER(f32div)       { BINARY_OP(f32, /); }
HANDLER(f64div)       { BINARY_OP(f64, /); }
HANDLER(f32min)       { MINMAX   (f32, std::min); }
HANDLER(f64min)       { MINMAX   (f64, std::min); }
HANDLER(f32max)       { MINMAX   (f32, std::max); }
HANDLER(f64max)       { MINMAX   (f64, std::max); }
HANDLER(f32copysign)  { BINARY_FN(f32, std::copysign); }
HANDLER(f64copysign)  { BINARY_FN(f64, std::copysign); }
HANDLER(i32wrap_i64)      { UNARY_OP(i64, (int32_t)); }
HANDLER(i64extend_i32_s)  { UNARY_OP(i32, (int64_t)); }
HANDLER(i64extend_i32_u)  { UNARY_OP(u32, (uint64_t)); }
HANDLER(i32trunc_f32_s)   { TRUNC   (f32, (int32_t),           -2147483777.,           2147483648.); }
HANDLER(i64trunc_f32_s)   { TRUNC   (f32, (int64_t),  -9223373136366404000.,  9223372036854776000.); }
HANDLER(i32trunc_f32_u)   { TRUNC   (f32, (uint32_t),                   -1.,           4294967296.); }
HANDLER(i64trunc_f32_u)   { TRUNC   (f32, (uint64_t),                   -1., 18446744073709552000.); }
HANDLER(i32trunc_f64_s)   { TRUNC   (f64, (int32_t),           -2147483649.,           2147483648.); }
HANDLER(i64trunc_f64_s)   { TRUNC   (f64, (int64_t),  -9223372036854777856.,  9223372036854776000.); }
HANDLER(i32trunc_f64_u)   { TRUNC   (f64, (uint32_t),                   -1.,           4294967296.); }
HANDLER(i64trunc_f64_u)   { TRUNC   (f64, (uint64_t),                   -1., 18446744073709552000.); }
HANDLER(f32convert_i32_s) { UNARY_OP(i32, (float)); }
HANDLER(f64convert_i32_s) { UNARY_OP(i32, (double)); }
HANDLER(f32convert_i32_u) { UNARY_OP(u32, (float)); }
HANDLER(f64convert_i32_u) { UNARY_OP(u32, (double)); }
HANDLER(f32convert_i64_s) { UNARY_OP(i64, (float)); }
HANDLER(f64convert_i64_s) { UNARY_OP(i64, (double)); }
HANDLER(f32convert_i64_u) { UNARY_OP(u64, (float)); }
HANDLER(f64convert_i64_u) { UNARY_OP(u64, (double)); }
HANDLER(f32demote_f64)    { UNARY_OP(f64, (float)); }
HANDLER(f64promote_f32)   { UNARY_OP(f32, (double)); }
HANDLER(i32extend8_s)  { UNARY_OP(i32, (int32_t)(int8_t)); }
HANDLER(i32extend16_s) { UNARY_OP(i32, (int32_t)(int16_t)); }
HANDLER(i64extend8_s)  { UNARY_OP(i64, (int64_t)(int8_t)); }
HANDLER(i64extend16_s) { UNARY_OP(i64, (int64_t)(int16_t)); }
HANDLER(i64extend32_s) { UNARY_OP(i64, (int64_t)(int32_t)); }
HANDLER(ref_null) {
    PRELUDE;
    *stack++ = (void*)nullptr;
    POSTLUDE;
}
HANDLER(ref_is_null) {
    PRELUDE;
    // note that funcref is also a full 0 value when null
    stack[-1].i32 = stack[-1].externref == nullptr;
    POSTLUDE;
}
HANDLER(ref_func) {
    // tmp1 = funcref index in misc table
    PRELUDE;
    auto funcref = MISC_GET(Funcref, tmp1);
    *stack++ = funcref;
    POSTLUDE;
}
// bitwise comparison applies to both
HANDLER(ref_eq) { BINARY_OP(externref, ==); }
HANDLER(i32_trunc_sat_f32_s) { TRUNC_SAT(f32, i32); }
HANDLER(i32_trunc_sat_f32_u) { TRUNC_SAT(f32, u32); }
HANDLER(i32_trunc_sat_f64_s) { TRUNC_SAT(f64, i32); }
HANDLER(i32_trunc_sat_f64_u) { TRUNC_SAT(f64, u32); }
HANDLER(i64_trunc_sat_f32_s) { TRUNC_SAT(f32, i64); }
HANDLER(i64_trunc_sat_f32_u) { TRUNC_SAT(f32, u64); }
HANDLER(i64_trunc_sat_f64_s) { TRUNC_SAT(f64, i64); }
HANDLER(i64_trunc_sat_f64_u) { TRUNC_SAT(f64, u64); }
// clang-format on
HANDLER(memory_init) {
    // tmp1 = segment address
    PRELUDE;
    auto &segment = *reinterpret_cast<Segment *>(tmp1);

    stack -= 3;
    auto size = stack[2].u32;
    auto src = stack[1].u32;
    auto dest = stack[0].u32;
    memory->copy_into(dest, src, segment, size);
    POSTLUDE;
}
HANDLER(memory_copy) {
    PRELUDE;
    stack -= 3;
    auto size = stack[2].u32;
    auto src = stack[1].u32;
    auto dst = stack[0].u32;
    memory->memcpy(dst, src, size);
    POSTLUDE;
}
HANDLER(memory_fill) {
    PRELUDE;
    stack -= 3;
    auto size = stack[2].u32;
    auto value = stack[1].u32;
    auto ptr = stack[0].u32;
    memory->memset(ptr, value, size);
    POSTLUDE;
}
HANDLER(table_init) {
    // tmp1 = element index in misc table
    // tmp2 = table index in misc table
    PRELUDE;
    auto &element = MISC_GET(ElementSegment, tmp1);
    auto &table = MISC_GET(WasmTable, tmp2);

    stack -= 3;
    auto size = stack[2].u32;
    auto src = stack[1].u32;
    auto dest = stack[0].u32;
    table.copy_into(dest, src, element, size);
    POSTLUDE;
}
HANDLER(elem_drop) {
    // tmp1 = element index in misc table
    PRELUDE;
    auto &element = MISC_GET(ElementSegment, tmp1);
    element.size = 0;
    element.elements = nullptr;
    POSTLUDE;
}
HANDLER(table_copy) {
    // tmp1 = destination table index in misc table
    // tmp2 = source table index in misc table
    PRELUDE;
    auto &dst_table = MISC_GET(WasmTable, tmp1);
    auto &src_table = MISC_GET(WasmTable, tmp2);

    stack -= 3;
    auto size = stack[2].u32;
    auto src = stack[1].u32;
    auto dst = stack[0].u32;
    src_table.memcpy(dst_table, dst, src, size);
    POSTLUDE;
}
HANDLER(table_grow) {
    // tmp1 = table index in misc table
    PRELUDE;
    auto &table = MISC_GET(WasmTable, tmp1);

    stack -= 1;
    auto delta = stack[0].u32;
    auto init = stack[-1];
    stack[-1] = table.grow(delta, init);
    POSTLUDE;
}
HANDLER(table_size) {
    // tmp1 = table index in misc table
    PRELUDE;
    auto &table = MISC_GET(WasmTable, tmp1);
    *stack++ = table.size();
    POSTLUDE;
}
HANDLER(table_fill) {
    // tmp1 = table index in misc table
    PRELUDE;
    auto &table = MISC_GET(WasmTable, tmp1);

    stack -= 3;
    auto size = stack[2].u32;
    auto value = stack[1];
    auto ptr = stack[0].u32;
    table.memset(ptr, value, size);
    POSTLUDE;
}

#undef STORE
#undef LOAD
#undef TRUNC_SAT
#undef SHIFT
#undef MINMAX
#undef IREM
#undef IDIV
#undef BINARY_FN
#undef BINARY_OP
#undef UNARY_FN
#undef TRUNC
#undef UNARY_OP

uint32_t WasmMemory::grow(uint32_t delta) {
    if (delta == 0)
        return current;
    // subtraction to avoid overflow
    if (delta > maximum - current) {
        return -1;
    }

    auto new_current = current + delta;
    auto new_memory =
        static_cast<uint8_t *>(realloc(memory, new_current * PAGE_SIZE));
    if (new_memory == NULL)
        return -1;
    memory = new_memory;
    std::memset(memory + current * PAGE_SIZE, 0, delta * PAGE_SIZE);

    auto old_current = current;
    current = new_current;
    return old_current;
}

void WasmMemory::copy_into(uint32_t dest, uint32_t src, const Segment &segment,
                           uint32_t length) {
    if (static_cast<uint64_t>(dest) + length > current * PAGE_SIZE ||
        src + length > segment.size) {
        trap("out of bounds memory access");
    }
    std::memcpy(memory + dest, segment.data.get() + src, length);
}

void WasmMemory::memcpy(uint32_t dst, uint32_t src, uint32_t length) {
    if (static_cast<uint64_t>(dst) + length > current * PAGE_SIZE ||
        static_cast<uint64_t>(src) + length > current * PAGE_SIZE) {
        trap("out of bounds memory access");
    }
    std::memmove(memory + dst, memory + src, length);
}

void WasmMemory::memset(uint32_t dst, uint8_t value, uint32_t length) {
    if (static_cast<uint64_t>(dst) + length > current * PAGE_SIZE) {
        trap("out of bounds memory access");
    }
    std::memset(memory + dst, value, length);
}

uint32_t WasmTable::grow(uint32_t delta, WasmValue value) {
    if (delta == 0)
        return current;
    // subtraction to avoid overflow
    if (delta > maximum - current) {
        return -1;
    }

    auto new_current = current + delta;
    auto new_elements = static_cast<WasmValue *>(
        realloc(elements, new_current * sizeof(WasmValue)));
    if (new_elements == NULL)
        return -1;
    elements = new_elements;
    std::fill(elements + current, elements + new_current, value);

    auto old_current = current;
    current = new_current;
    return old_current;
}

WasmValue WasmTable::get(uint32_t idx) {
    if (idx >= current) {
        trap("out of bounds table access");
    }
    return elements[idx];
}

void WasmTable::set(uint32_t idx, WasmValue value) {
    if (idx >= current) {
        trap("out of bounds table access");
    }
    elements[idx] = value;
}

void WasmTable::copy_into(uint32_t dst, uint32_t src,
                          const ElementSegment &segment, uint32_t length) {
    if (static_cast<uint64_t>(dst) + length > current ||
        src + length > segment.size) {
        trap("out of bounds table access");
    }
    std::memcpy(elements + dst, segment.elements.get() + src,
                length * sizeof(WasmValue));
}

void WasmTable::memcpy(WasmTable &dst_table, uint32_t dst, uint32_t src,
                       uint32_t length) {
    if (static_cast<uint64_t>(dst) + length > dst_table.current ||
        static_cast<uint64_t>(src) + length > this->current) {
        trap("out of bounds table access");
    }
    std::memmove(dst_table.elements + dst, elements + src,
                 length * sizeof(WasmValue));
}

void WasmTable::memset(uint32_t dst, WasmValue value, uint32_t length) {
    if (static_cast<uint64_t>(dst) + length > current) {
        trap("out of bounds table access");
    }
    std::fill(elements + dst, elements + dst + length, value);
}

}; // namespace runtime
}; // namespace mitey
