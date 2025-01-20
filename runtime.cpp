#include "runtime.hpp"
#include <algorithm>
#include <bit>
#include <cmath>

#ifdef WASM_DEBUG
extern std::vector<std::string> names;
#endif

namespace mitey {
namespace runtime {

// manual implementation because std::rotl/rotr had weird codegen
template <typename T> T rotl(T x, T n) {
    n &= sizeof(T) * 8 - 1;
    return (x << n) | (x >> (sizeof(T) * 8 - n));
}

template <typename T> T rotr(T x, T n) {
    n &= sizeof(T) * 8 - 1;
    return (x >> n) | (x << (sizeof(T) * 8 - n));
}

Segment Segment::empty(0, 0, nullptr, nullptr);
WasmMemory WasmMemory::empty = WasmMemory();
Allocation (*WasmMemory::default_make_memory)(size_t, AllocationKind) = nullptr;
int (*WasmMemory::default_grow_memory)(WasmMemory &, size_t) = nullptr;

std::jmp_buf *trap_buf;
uint32_t call_stack_depth = 10000;

#define HANDLER(name)                                                          \
    void name(uint8_t *memory, void **misc, WasmValue *stack, uint64_t tmp1,   \
              uint64_t tmp2)
#define TEMPLESS_PARAMS memory, misc, stack
#define PARAMS TEMPLESS_PARAMS, tmp1, tmp2
#define PRELUDE auto &memheader = MISC_GET(WasmMemory, 0)
#define POSTLUDE [[clang::musttail]] return dummy(PARAMS)
#define MISC_GET(type, idx) (*reinterpret_cast<type *>(misc[idx]))
#define byteadd(ptr, n)                                                        \
    reinterpret_cast<decltype(ptr)>(reinterpret_cast<uint64_t>(ptr) +          \
                                    static_cast<uint64_t>(n))

__attribute__((noinline)) void dummy(uint8_t *memory, void **misc,
                                     WasmValue *stack, uint64_t, uint64_t) {
    // assumption: compiler doesn't move memory/stack/misc from arg registers
    asm volatile("" ::"r"(memory), "r"(misc), "r"(stack));
    return;
}

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-parameter"

// non-instruction handlers
HANDLER(clear_locals) {
    // tmp1 = non-parameter local bytes
    PRELUDE;
    std::memset(stack, 0, tmp1);
    stack = byteadd(stack, tmp1);
    POSTLUDE;
}
template <ssize_t N = -1> HANDLER(base_move_results) {
    // tmp1 = # of locals
    // tmp2 = # of results
    PRELUDE;
    auto n_locals = static_cast<int64_t>(tmp1);
    auto n_results = static_cast<int64_t>(N == -1 ? tmp2 : -N);
    // memmove because results > locals is possible
    std::memmove(byteadd(stack, n_locals + n_results),
                 byteadd(stack, n_results), -n_results);
    stack = byteadd(stack, n_locals);
    POSTLUDE;
}
HANDLER(move_0_results) { base_move_results<0>(PARAMS); }
HANDLER(move_8_results) { base_move_results<8>(PARAMS); }
HANDLER(move_n_results) { base_move_results<>(PARAMS); }

HANDLER(jump) {
    // tmp1 = target
    PRELUDE;
    [[clang::musttail]] return reinterpret_cast<Signature *>(tmp1)(PARAMS);
}

HANDLER(unreachable) { trap(TrapKind::unreachable); }
HANDLER(if_) {
    // tmp1 = else branch
    PRELUDE;
    --stack;
    if (!stack->u32) {
        [[clang::musttail]] return reinterpret_cast<Signature *>(tmp1)(PARAMS);
    }
    POSTLUDE;
}
template <ssize_t Arity = -1, ssize_t StackOffset = -1> HANDLER(base_br) {
    // tmp1 = target
    // tmp2 = brinfo
    PRELUDE;
    auto info = std::bit_cast<BrInfo>(tmp2);
    auto arity = Arity == -1 ? info.arity : Arity;
    auto stack_offset = StackOffset == -1 ? info.stack_offset : -StackOffset;

    std::memmove(byteadd(stack, stack_offset), byteadd(stack, -arity), arity);
    stack = byteadd(stack, stack_offset + arity);
    [[clang::musttail]] return reinterpret_cast<Signature *>(tmp1)(PARAMS);
}
// generated based on the most common in figma/duckdb binaries
HANDLER(br_0_0) { [[clang::musttail]] return base_br<0, 0>(PARAMS); }
HANDLER(br_0_8) { [[clang::musttail]] return base_br<0, 8>(PARAMS); }
HANDLER(br_0_16) { [[clang::musttail]] return base_br<0, 16>(PARAMS); }
HANDLER(br_0_24) { [[clang::musttail]] return base_br<0, 24>(PARAMS); }
HANDLER(br_0_32) { [[clang::musttail]] return base_br<0, 32>(PARAMS); }
HANDLER(br_0_40) { [[clang::musttail]] return base_br<0, 40>(PARAMS); }
HANDLER(br_8_8) { [[clang::musttail]] return base_br<8, 8>(PARAMS); }
HANDLER(br_8_16) { [[clang::musttail]] return base_br<8, 16>(PARAMS); }
HANDLER(br_8_24) { [[clang::musttail]] return base_br<8, 24>(PARAMS); }
HANDLER(br_n_n) { [[clang::musttail]] return base_br<>(PARAMS); }

template <ssize_t Arity = -1, ssize_t StackOffset = -1> HANDLER(base_br_if) {
    // tmp1 = target
    // tmp2 = brinfo
    PRELUDE;
    --stack;
    if (stack->u32) {
        [[clang::musttail]] return base_br<Arity, StackOffset>(PARAMS);
    }
    POSTLUDE;
}
HANDLER(br_if_0_0) { [[clang::musttail]] return base_br_if<0, 0>(PARAMS); }
HANDLER(br_if_0_8) { [[clang::musttail]] return base_br_if<0, 8>(PARAMS); }
HANDLER(br_if_0_16) { [[clang::musttail]] return base_br_if<0, 16>(PARAMS); }
HANDLER(br_if_0_24) { [[clang::musttail]] return base_br_if<0, 24>(PARAMS); }
HANDLER(br_if_0_32) { [[clang::musttail]] return base_br_if<0, 32>(PARAMS); }
HANDLER(br_if_0_40) { [[clang::musttail]] return base_br_if<0, 40>(PARAMS); }
HANDLER(br_if_8_8) { [[clang::musttail]] return base_br_if<8, 8>(PARAMS); }
HANDLER(br_if_8_16) { [[clang::musttail]] return base_br_if<8, 16>(PARAMS); }
HANDLER(br_if_8_24) { [[clang::musttail]] return base_br_if<8, 24>(PARAMS); }
HANDLER(br_if_n_n) { [[clang::musttail]] return base_br_if<>(PARAMS); }

template <ssize_t Arity = -1> HANDLER(br_table) {
    // tmp1 = lookup table addr
    // tmp2 = brinfo
    PRELUDE;
    auto lookup = reinterpret_cast<BrTableTarget *>(tmp1);
    auto info = std::bit_cast<BrInfo>(tmp2);
    --stack;
    auto jump = std::min(static_cast<uint32_t>(info.n_targets), stack->u32);
    auto target = lookup[jump];
    info.stack_offset = target.stack_offset;
    tmp1 /* dest */ = reinterpret_cast<uint64_t>(lookup) + target.lookup_offset;
    tmp2 = std::bit_cast<uint64_t>(info);
    [[clang::musttail]] return base_br<Arity>(PARAMS);
}
HANDLER(br_table_0) { [[clang::musttail]] return br_table<0>(PARAMS); }
HANDLER(br_table_8) { [[clang::musttail]] return br_table<8>(PARAMS); }
HANDLER(br_table_n) { [[clang::musttail]] return br_table<>(PARAMS); }

HANDLER(call) {
    // tmp1 = function start
    PRELUDE;
    call_stack_depth--;
    if (call_stack_depth == 0)
        trap(TrapKind::call_stack_exhausted);

    reinterpret_cast<TemplessSignature *>(tmp1)(TEMPLESS_PARAMS);
    stack = byteadd(stack, tmp2);

    call_stack_depth++;
    POSTLUDE;
}
HANDLER(call_extern) {
    // tmp1 = function offset in misc
    PRELUDE;
    call_stack_depth--;
    if (call_stack_depth == 0)
        trap(TrapKind::call_stack_exhausted);

    auto &func = MISC_GET(FunctionInfo, tmp1);
    func.signature(func.memory, func.misc, stack);
    stack = byteadd(stack, tmp2);

    call_stack_depth++;
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
        trap(TrapKind::undefined_element);
    }
    auto funcref = table.get(elem_idx).funcref;
    if (!funcref) {
        trap(TrapKind::uninitialized_element);
    }
    if (funcref->type != info.type) {
        trap(TrapKind::indirect_call_type_mismatch);
    }

    call_stack_depth--;
    if (call_stack_depth == 0)
        trap(TrapKind::call_stack_exhausted);

    funcref->signature(funcref->memory, funcref->misc, stack);
    stack = byteadd(stack, sizeof(runtime::WasmValue) *
                               (funcref->type.results - funcref->type.params));

    call_stack_depth++;
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
    auto &local = *byteadd(stack, tmp1);
    *stack++ = local;
    POSTLUDE;
}
HANDLER(localset) {
    // tmp1 = local index/stack offset to local
    PRELUDE;
    auto &local = *byteadd(stack, tmp1);
    local = *--stack;
    POSTLUDE;
}
HANDLER(localtee) {
    // tmp1 = local index/stack offset to local
    PRELUDE;
    *byteadd(stack, tmp1) = stack[-1];
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
    auto &global = MISC_GET(WasmValue, tmp1);
    *stack++ = global;
    POSTLUDE;
}
HANDLER(globalset) {
    // tmp1 = global index in misc table
    PRELUDE;
    auto &global = MISC_GET(WasmValue, tmp1);
    global = *--stack;
    POSTLUDE;
}
HANDLER(memorysize) {
    PRELUDE;
    *stack++ = memheader.size();
    POSTLUDE;
}
HANDLER(memorygrow) {
    PRELUDE;
    stack[-1].u32 = memheader.grow(stack[-1].u32);
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
                trap(TrapKind::invalid_conversion_to_integer);                 \
            } else {                                                           \
                trap(TrapKind::integer_overflow);                              \
            }                                                                  \
        }                                                                      \
        if (stack[-1].type <= lower || upper <= stack[-1].type) {              \
            trap(TrapKind::integer_overflow);                                  \
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
            trap(TrapKind::integer_divide_by_zero);                            \
        }                                                                      \
        if (std::is_signed_v<type> &&                                          \
            stack[0].type == static_cast<type>(-1) &&                          \
            stack[-1].type == std::numeric_limits<type>::min()) {              \
            trap(TrapKind::integer_overflow);                                  \
        }                                                                      \
        stack[-1] = stack[-1].type / stack[0].type;                            \
        POSTLUDE;                                                              \
    }
#define IREM(type)                                                             \
    {                                                                          \
        PRELUDE;                                                               \
        stack--;                                                               \
        if (stack[0].type == 0) {                                              \
            trap(TrapKind::integer_divide_by_zero);                            \
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
        auto src = memory + stack[-1].u32 + tmp1;                              \
        memtype value;                                                         \
        std::memcpy(&value, src, sizeof(memtype));                             \
        stack[-1].stacktype = value;                                           \
        POSTLUDE;                                                              \
    }

#define STORE(stacktype, memtype)                                              \
    {                                                                          \
        /* tmp1 = offset */                                                    \
        PRELUDE;                                                               \
        stack -= 2;                                                            \
        auto dst = memory + stack[0].u32 + tmp1;                               \
        /* make sure all bytes are in bounds before writing (on -O0) */        \
        for (auto i = 0; i < sizeof(memtype); i++)                             \
            dst[i] = dst[i];                                                   \
        memtype value = stack[1].stacktype;                                    \
        std::memcpy(dst, &value, sizeof(memtype));                             \
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
HANDLER(i32rotl)      { BINARY_FN(u32, rotl); }
HANDLER(i64rotl)      { BINARY_FN(u64, rotl); }
HANDLER(i32rotr)      { BINARY_FN(u32, rotr); }
HANDLER(i64rotr)      { BINARY_FN(u64, rotr); }
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
    *stack++ = (Funcref)nullptr;
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
    // & because we want the address of the funcref
    auto funcref = &MISC_GET(FunctionInfo, tmp1);
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
    // tmp1 = segment index in misc table
    PRELUDE;
    auto &segment = MISC_GET(Segment, tmp1);

    stack -= 3;
    auto size = stack[2].u32;
    auto src = stack[1].u32;
    auto dest = stack[0].u32;
    memheader.copy_into(dest, src, segment, size);
    POSTLUDE;
}
HANDLER(data_drop) {
    // tmp1 = segment index in misc table
    PRELUDE;
    // should there be some way for MISC_GET to do this?
    misc[tmp1] = &Segment::empty;
    POSTLUDE;
}
HANDLER(memory_copy) {
    PRELUDE;
    stack -= 3;
    auto size = stack[2].u32;
    auto src = stack[1].u32;
    auto dst = stack[0].u32;
    memheader.memcpy(dst, src, size);
    POSTLUDE;
}
HANDLER(memory_fill) {
    PRELUDE;
    stack -= 3;
    auto size = stack[2].u32;
    auto value = stack[1].u32;
    auto ptr = stack[0].u32;
    memheader.memset(ptr, value, size);
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

    auto result = grow_memory(*this, delta);
    if (result == -1)
        return -1;

    auto old_current = current;
    current += delta;
    return old_current;
}

void WasmMemory::copy_into(uint32_t dest, uint32_t src, const Segment &segment,
                           uint32_t length) {
    if (static_cast<uint64_t>(dest) + length > current * PAGE_SIZE ||
        src + length > segment.size) {
        trap(TrapKind::out_of_bounds_memory_access);
    }
    std::memcpy(memory.get() + dest, segment.data.get() + src, length);
}

void WasmMemory::memcpy(uint32_t dst, uint32_t src, uint32_t length) {
    if (static_cast<uint64_t>(dst) + length > current * PAGE_SIZE ||
        static_cast<uint64_t>(src) + length > current * PAGE_SIZE) {
        trap(TrapKind::out_of_bounds_memory_access);
    }
    std::memmove(memory.get() + dst, memory.get() + src, length);
}

void WasmMemory::memset(uint32_t dst, uint8_t value, uint32_t length) {
    if (static_cast<uint64_t>(dst) + length > current * PAGE_SIZE) {
        trap(TrapKind::out_of_bounds_memory_access);
    }
    std::memset(memory.get() + dst, value, length);
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
        trap(TrapKind::out_of_bounds_table_access);
    }
    return elements[idx];
}

void WasmTable::set(uint32_t idx, WasmValue value) {
    if (idx >= current) {
        trap(TrapKind::out_of_bounds_table_access);
    }
    elements[idx] = value;
}

void WasmTable::copy_into(uint32_t dst, uint32_t src,
                          const ElementSegment &segment, uint32_t length) {
    if (static_cast<uint64_t>(dst) + length > current ||
        src + length > segment.size) {
        trap(TrapKind::out_of_bounds_table_access);
    }
    std::memcpy(elements + dst, segment.elements.get() + src,
                length * sizeof(WasmValue));
}

void WasmTable::memcpy(WasmTable &dst_table, uint32_t dst, uint32_t src,
                       uint32_t length) {
    if (static_cast<uint64_t>(dst) + length > dst_table.current ||
        static_cast<uint64_t>(src) + length > this->current) {
        trap(TrapKind::out_of_bounds_table_access);
    }
    std::memmove(dst_table.elements + dst, elements + src,
                 length * sizeof(WasmValue));
}

void WasmTable::memset(uint32_t dst, WasmValue value, uint32_t length) {
    if (static_cast<uint64_t>(dst) + length > current) {
        trap(TrapKind::out_of_bounds_table_access);
    }
    std::fill(elements + dst, elements + dst + length, value);
}

#pragma clang diagnostic pop

}; // namespace runtime
}; // namespace mitey
