#pragma once

#include "type-templates.hpp"
#include <cstdint>
#include <stdexcept>
#include <string>
#include <vector>

namespace mitey {
template <typename Err>
[[noreturn]] static void __attribute__((preserve_most)) error(const char *msg) {
    throw Err(msg);
}

class malformed_error : public std::runtime_error {
  public:
    malformed_error(const std::string &msg) : std::runtime_error(msg) {}
};

class validation_error : public std::runtime_error {
  public:
    validation_error(const std::string &msg) : std::runtime_error(msg) {}
};

class trap_error : public std::runtime_error {
  public:
    trap_error(const std::string &message) : std::runtime_error(message) {}
};

class link_error : public std::runtime_error {
  public:
    link_error(const std::string &message) : std::runtime_error(message) {}
};

class uninstantiable_error : public std::runtime_error {
  public:
    uninstantiable_error(const std::string &message)
        : std::runtime_error(message) {}
};

enum class valtype : uint8_t {
    null = 0x00,
    any = 0xff,

    // numtype
    i32 = 0x7f,
    i64 = 0x7e,
    f32 = 0x7d,
    f64 = 0x7c,

    // // vectype
    // v128 = 0x7b,

    // reftype
    funcref = 0x70,
    externref = 0x6f,
};

template <typename T> static constexpr auto Valtype = valtype::null;
template <> static constexpr auto Valtype<int32_t> = valtype::i32;
template <> static constexpr auto Valtype<uint32_t> = valtype::i32;
template <> static constexpr auto Valtype<int64_t> = valtype::i64;
template <> static constexpr auto Valtype<uint64_t> = valtype::i64;
template <> static constexpr auto Valtype<float> = valtype::f32;
template <> static constexpr auto Valtype<double> = valtype::f64;
template <typename T> static constexpr auto Valtype<T *> = valtype::externref;

#ifdef WASM_DEBUG
static std::string valtype_names[] = {
    [static_cast<uint8_t>(valtype::null)] = "null",
    [static_cast<uint8_t>(valtype::any)] = "any",
    [static_cast<uint8_t>(valtype::i32)] = "i32",
    [static_cast<uint8_t>(valtype::i64)] = "i64",
    [static_cast<uint8_t>(valtype::f32)] = "f32",
    [static_cast<uint8_t>(valtype::f64)] = "f64",
    [static_cast<uint8_t>(valtype::funcref)] = "funcref",
    [static_cast<uint8_t>(valtype::externref)] = "externref"};
#endif

static inline uint32_t valtype_size(valtype type) {
    switch (type) {
    case valtype::any:
    case valtype::null:
        return 0;
    case valtype::i32:
    case valtype::i64:
    case valtype::f32:
    case valtype::f64:
    case valtype::funcref:
    case valtype::externref:
        return 1;
    }
}

[[noreturn]] static inline void trap(const char *message) {
    error<trap_error>(message);
}

static inline bool is_reftype(uint32_t byte) {
    return byte == static_cast<uint8_t>(valtype::funcref) ||
           byte == static_cast<uint8_t>(valtype::externref);
}

static inline bool is_reftype(valtype type) {
    return is_reftype(static_cast<uint8_t>(type));
}

static inline bool is_numtype(uint32_t byte) {
    return byte == static_cast<uint8_t>(valtype::i32) ||
           byte == static_cast<uint8_t>(valtype::i64) ||
           byte == static_cast<uint8_t>(valtype::f32) ||
           byte == static_cast<uint8_t>(valtype::f64);
}

static inline bool is_numtype(valtype type) {
    return is_numtype(static_cast<uint8_t>(type));
}

static inline bool is_valtype(uint32_t byte) {
    return is_numtype(byte) || is_reftype(byte);
}

static inline bool is_valtype(valtype type) {
    return is_valtype(static_cast<uint8_t>(type));
}

struct WasmSignature {
    std::vector<valtype> params;
    std::vector<valtype> results;

    template <typename Func> static WasmSignature from_type() {
        using Traits = function_traits<Func>;
        using Args = typename Traits::args;
        using ReturnType = typename Traits::return_type;

        WasmSignature sig;

        sig.params.reserve(Traits::parameter_arity);
        fold_vector<Args>(sig.params,
                          std::make_index_sequence<Traits::parameter_arity>{});

        sig.results.reserve(Traits::result_arity);
        fold_vector<ReturnType>(
            sig.results, std::make_index_sequence<Traits::result_arity>{});

        return sig;
    }

  private:
    template <typename Args, size_t... Is>
    static void fold_vector(std::vector<valtype> &vec,
                            std::index_sequence<Is...>) {
        (vec.push_back(Valtype<std::tuple_element_t<Is, Args>>), ...);
    }
};

enum class mut {
    const_ = 0x0,
    var = 0x1,
};

static inline bool is_mut(uint8_t byte) {
    return byte == static_cast<uint8_t>(mut::const_) ||
           byte == static_cast<uint8_t>(mut::var);
}

// from https://stackoverflow.com/a/28311607
static inline bool is_valid_utf8(const uint8_t *bytes, size_t length) {
    if (!bytes)
        return true;

    const uint8_t *end = bytes + length;
    unsigned int cp;
    int num;

    while (bytes < end) {
        if ((*bytes & 0x80) == 0x00) {
            // U+0000 to U+007F
            cp = (*bytes & 0x7F);
            num = 1;
        } else if ((*bytes & 0xE0) == 0xC0) {
            // U+0080 to U+07FF
            cp = (*bytes & 0x1F);
            num = 2;
        } else if ((*bytes & 0xF0) == 0xE0) {
            // U+0800 to U+FFFF
            cp = (*bytes & 0x0F);
            num = 3;
        } else if ((*bytes & 0xF8) == 0xF0) {
            // U+10000 to U+10FFFF
            cp = (*bytes & 0x07);
            num = 4;
        } else
            return false;

        if (bytes + num > end)
            return false;

        bytes += 1;
        for (int i = 1; i < num; ++i) {
            if ((*bytes & 0xC0) != 0x80)
                return false;
            cp = (cp << 6) | (*bytes & 0x3F);
            bytes += 1;
        }

        if ((cp > 0x10FFFF) || ((cp >= 0xD800) && (cp <= 0xDFFF)) ||
            ((cp <= 0x007F) && (num != 1)) ||
            ((cp >= 0x0080) && (cp <= 0x07FF) && (num != 2)) ||
            ((cp >= 0x0800) && (cp <= 0xFFFF) && (num != 3)) ||
            ((cp >= 0x10000) && (cp <= 0x1FFFFF) && (num != 4)))
            return false;
    }

    return bytes == end;
}

template <typename T, uint8_t BITS = sizeof(T) * 8, typename Iter>
static inline T safe_read_sleb128(Iter &iter) {
    auto start = iter;

    int64_t result = 0;
    uint64_t shift = 0;
    uint64_t byte;
    do {
        byte = *iter++;
        result |= (byte & 0x7f) << shift;
        shift += 7;
    } while (byte & 0x80);
    if (shift < 64 && (byte & 0x40)) {
        result |= static_cast<int64_t>(-1) << shift;
    }

    if (result > static_cast<int64_t>((1ULL << (BITS - 1)) - 1)) {
        error<malformed_error>("integer too large");
    }
    if (result < static_cast<int64_t>(-(1ULL << (BITS - 1)))) {
        error<malformed_error>("integer too large");
    }
    if (static_cast<uint64_t>(iter - start) >
        static_cast<uint64_t>(1 + BITS / 7)) {
        error<malformed_error>("integer representation too long");
    }
    if (((iter[-1] != 0 && iter[-1] != 127) + (iter - start - 1) * 7) >= BITS) {
        error<malformed_error>("integer too large");
    }
    return static_cast<T>(result);
}

template <typename T, uint8_t BITS = sizeof(T) * 8, typename Iter>
static inline T safe_read_leb128(Iter &iter) {
    auto start = iter;

    uint64_t result = 0;
    uint64_t shift = 0;
    uint64_t byte;
    do {
        byte = *iter++;
        result |= (byte & 0x7f) << shift;
        shift += 7;
    } while (byte & 0x80);

    if (static_cast<uint64_t>(iter - start) >
        static_cast<uint64_t>(1 + BITS / 7)) {
        error<malformed_error>("integer representation too long");
    }
    if (sizeof(T) != 8 && result > (1ULL << BITS) - 1) {
        error<malformed_error>("integer too large");
    }
    return static_cast<T>(result);
}

#define FOREACH_INSTRUCTION(V)                                                 \
    V(unreachable, "unreachable", 0x00)                                        \
    V(nop, "nop", 0x01)                                                        \
    V(block, "block", 0x02)                                                    \
    V(loop, "loop", 0x03)                                                      \
    V(if_, "if", 0x04)                                                         \
    V(else_, "else", 0x05)                                                     \
    V(end, "end", 0x0b)                                                        \
    V(br, "br", 0x0c)                                                          \
    V(br_if, "br_if", 0x0d)                                                    \
    V(br_table, "br_table", 0x0e)                                              \
    V(return_, "return", 0x0f)                                                 \
    V(call, "call", 0x10)                                                      \
    V(call_indirect, "call_indirect", 0x11)                                    \
    V(drop, "drop", 0x1a)                                                      \
    V(select, "select", 0x1b)                                                  \
    V(select_t, "select_t", 0x1c)                                              \
    V(localget, "local.get", 0x20)                                             \
    V(localset, "local.set", 0x21)                                             \
    V(localtee, "local.tee", 0x22)                                             \
    V(globalget, "global.get", 0x23)                                           \
    V(globalset, "global.set", 0x24)                                           \
    V(tableget, "table.get", 0x25)                                             \
    V(tableset, "table.set", 0x26)                                             \
    V(i32load, "i32.load", 0x28)                                               \
    V(i64load, "i64.load", 0x29)                                               \
    V(f32load, "f32.load", 0x2a)                                               \
    V(f64load, "f64.load", 0x2b)                                               \
    V(i32load8_s, "i32.load8_s", 0x2c)                                         \
    V(i32load8_u, "i32.load8_u", 0x2d)                                         \
    V(i32load16_s, "i32.load16_s", 0x2e)                                       \
    V(i32load16_u, "i32.load16_u", 0x2f)                                       \
    V(i64load8_s, "i64.load8_s", 0x30)                                         \
    V(i64load8_u, "i64.load8_u", 0x31)                                         \
    V(i64load16_s, "i64.load16_s", 0x32)                                       \
    V(i64load16_u, "i64.load16_u", 0x33)                                       \
    V(i64load32_s, "i64.load32_s", 0x34)                                       \
    V(i64load32_u, "i64.load32_u", 0x35)                                       \
    V(i32store, "i32.store", 0x36)                                             \
    V(i64store, "i64.store", 0x37)                                             \
    V(f32store, "f32.store", 0x38)                                             \
    V(f64store, "f64.store", 0x39)                                             \
    V(i32store8, "i32.store8", 0x3a)                                           \
    V(i32store16, "i32.store16", 0x3b)                                         \
    V(i64store8, "i64.store8", 0x3c)                                           \
    V(i64store16, "i64.store16", 0x3d)                                         \
    V(i64store32, "i64.store32", 0x3e)                                         \
    V(memorysize, "memory.size", 0x3f)                                         \
    V(memorygrow, "memory.grow", 0x40)                                         \
    V(i32const, "i32.const", 0x41)                                             \
    V(i64const, "i64.const", 0x42)                                             \
    V(f32const, "f32.const", 0x43)                                             \
    V(f64const, "f64.const", 0x44)                                             \
    V(i32eqz, "i32.eqz", 0x45)                                                 \
    V(i64eqz, "i64.eqz", 0x50)                                                 \
    V(i32eq, "i32.eq", 0x46)                                                   \
    V(i64eq, "i64.eq", 0x51)                                                   \
    V(i32ne, "i32.ne", 0x47)                                                   \
    V(i64ne, "i64.ne", 0x52)                                                   \
    V(i32lt_s, "i32.lt_s", 0x48)                                               \
    V(i64lt_s, "i64.lt_s", 0x53)                                               \
    V(i32lt_u, "i32.lt_u", 0x49)                                               \
    V(i64lt_u, "i64.lt_u", 0x54)                                               \
    V(i32gt_s, "i32.gt_s", 0x4a)                                               \
    V(i64gt_s, "i64.gt_s", 0x55)                                               \
    V(i32gt_u, "i32.gt_u", 0x4b)                                               \
    V(i64gt_u, "i64.gt_u", 0x56)                                               \
    V(i32le_s, "i32.le_s", 0x4c)                                               \
    V(i64le_s, "i64.le_s", 0x57)                                               \
    V(i32le_u, "i32.le_u", 0x4d)                                               \
    V(i64le_u, "i64.le_u", 0x58)                                               \
    V(i32ge_s, "i32.ge_s", 0x4e)                                               \
    V(i64ge_s, "i64.ge_s", 0x59)                                               \
    V(i32ge_u, "i32.ge_u", 0x4f)                                               \
    V(i64ge_u, "i64.ge_u", 0x5a)                                               \
    V(f32eq, "f32.eq", 0x5b)                                                   \
    V(f64eq, "f64.eq", 0x61)                                                   \
    V(f32ne, "f32.ne", 0x5c)                                                   \
    V(f64ne, "f64.ne", 0x62)                                                   \
    V(f32lt, "f32.lt", 0x5d)                                                   \
    V(f64lt, "f64.lt", 0x63)                                                   \
    V(f32gt, "f32.gt", 0x5e)                                                   \
    V(f64gt, "f64.gt", 0x64)                                                   \
    V(f32le, "f32.le", 0x5f)                                                   \
    V(f64le, "f64.le", 0x65)                                                   \
    V(f32ge, "f32.ge", 0x60)                                                   \
    V(f64ge, "f64.ge", 0x66)                                                   \
    V(i32clz, "i32.clz", 0x67)                                                 \
    V(i64clz, "i64.clz", 0x79)                                                 \
    V(i32ctz, "i32.ctz", 0x68)                                                 \
    V(i64ctz, "i64.ctz", 0x7a)                                                 \
    V(i32popcnt, "i32.popcnt", 0x69)                                           \
    V(i64popcnt, "i64.popcnt", 0x7b)                                           \
    V(i32add, "i32.add", 0x6a)                                                 \
    V(i64add, "i64.add", 0x7c)                                                 \
    V(i32sub, "i32.sub", 0x6b)                                                 \
    V(i64sub, "i64.sub", 0x7d)                                                 \
    V(i32mul, "i32.mul", 0x6c)                                                 \
    V(i64mul, "i64.mul", 0x7e)                                                 \
    V(i32div_s, "i32.div_s", 0x6d)                                             \
    V(i64div_s, "i64.div_s", 0x7f)                                             \
    V(i32div_u, "i32.div_u", 0x6e)                                             \
    V(i64div_u, "i64.div_u", 0x80)                                             \
    V(i32rem_s, "i32.rem_s", 0x6f)                                             \
    V(i64rem_s, "i64.rem_s", 0x81)                                             \
    V(i32rem_u, "i32.rem_u", 0x70)                                             \
    V(i64rem_u, "i64.rem_u", 0x82)                                             \
    V(i32and, "i32.and", 0x71)                                                 \
    V(i64and, "i64.and", 0x83)                                                 \
    V(i32or, "i32.or", 0x72)                                                   \
    V(i64or, "i64.or", 0x84)                                                   \
    V(i32xor, "i32.xor", 0x73)                                                 \
    V(i64xor, "i64.xor", 0x85)                                                 \
    V(i32shl, "i32.shl", 0x74)                                                 \
    V(i64shl, "i64.shl", 0x86)                                                 \
    V(i32shr_s, "i32.shr_s", 0x75)                                             \
    V(i64shr_s, "i64.shr_s", 0x87)                                             \
    V(i32shr_u, "i32.shr_u", 0x76)                                             \
    V(i64shr_u, "i64.shr_u", 0x88)                                             \
    V(i32rotl, "i32.rotl", 0x77)                                               \
    V(i64rotl, "i64.rotl", 0x89)                                               \
    V(i32rotr, "i32.rotr", 0x78)                                               \
    V(i64rotr, "i64.rotr", 0x8a)                                               \
    V(f32abs, "f32.abs", 0x8b)                                                 \
    V(f64abs, "f64.abs", 0x99)                                                 \
    V(f32neg, "f32.neg", 0x8c)                                                 \
    V(f64neg, "f64.neg", 0x9a)                                                 \
    V(f32ceil, "f32.ceil", 0x8d)                                               \
    V(f64ceil, "f64.ceil", 0x9b)                                               \
    V(f32floor, "f32.floor", 0x8e)                                             \
    V(f64floor, "f64.floor", 0x9c)                                             \
    V(f32trunc, "f32.trunc", 0x8f)                                             \
    V(f64trunc, "f64.trunc", 0x9d)                                             \
    V(f32nearest, "f32.nearest", 0x90)                                         \
    V(f64nearest, "f64.nearest", 0x9e)                                         \
    V(f32sqrt, "f32.sqrt", 0x91)                                               \
    V(f64sqrt, "f64.sqrt", 0x9f)                                               \
    V(f32add, "f32.add", 0x92)                                                 \
    V(f64add, "f64.add", 0xa0)                                                 \
    V(f32sub, "f32.sub", 0x93)                                                 \
    V(f64sub, "f64.sub", 0xa1)                                                 \
    V(f32mul, "f32.mul", 0x94)                                                 \
    V(f64mul, "f64.mul", 0xa2)                                                 \
    V(f32div, "f32.div", 0x95)                                                 \
    V(f64div, "f64.div", 0xa3)                                                 \
    V(f32min, "f32.min", 0x96)                                                 \
    V(f64min, "f64.min", 0xa4)                                                 \
    V(f32max, "f32.max", 0x97)                                                 \
    V(f64max, "f64.max", 0xa5)                                                 \
    V(f32copysign, "f32.copysign", 0x98)                                       \
    V(f64copysign, "f64.copysign", 0xa6)                                       \
    V(i32wrap_i64, "i32.wrap_i64", 0xa7)                                       \
    V(i64extend_i32_s, "i64.extend_i32_s", 0xac)                               \
    V(i64extend_i32_u, "i64.extend_i32_u", 0xad)                               \
    V(i32trunc_f32_s, "i32.trunc_f32_s", 0xa8)                                 \
    V(i64trunc_f32_s, "i64.trunc_f32_s", 0xae)                                 \
    V(i32trunc_f32_u, "i32.trunc_f32_u", 0xa9)                                 \
    V(i64trunc_f32_u, "i64.trunc_f32_u", 0xaf)                                 \
    V(i32trunc_f64_s, "i32.trunc_f64_s", 0xaa)                                 \
    V(i64trunc_f64_s, "i64.trunc_f64_s", 0xb0)                                 \
    V(i32trunc_f64_u, "i32.trunc_f64_u", 0xab)                                 \
    V(i64trunc_f64_u, "i64.trunc_f64_u", 0xb1)                                 \
    V(f32convert_i32_s, "f32.convert_i32_s", 0xb2)                             \
    V(f64convert_i32_s, "f64.convert_i32_s", 0xb7)                             \
    V(f32convert_i32_u, "f32.convert_i32_u", 0xb3)                             \
    V(f64convert_i32_u, "f64.convert_i32_u", 0xb8)                             \
    V(f32convert_i64_s, "f32.convert_i64_s", 0xb4)                             \
    V(f64convert_i64_s, "f64.convert_i64_s", 0xb9)                             \
    V(f32convert_i64_u, "f32.convert_i64_u", 0xb5)                             \
    V(f64convert_i64_u, "f64.convert_i64_u", 0xba)                             \
    V(f32demote_f64, "f32.demote_f64", 0xb6)                                   \
    V(f64promote_f32, "f64.promote_f32", 0xbb)                                 \
    V(i32reinterpret_f32, "i32.reinterpret_f32", 0xbc)                         \
    V(f32reinterpret_i32, "f32.reinterpret_i32", 0xbe)                         \
    V(i64reinterpret_f64, "i64.reinterpret_f64", 0xbd)                         \
    V(f64reinterpret_i64, "f64.reinterpret_i64", 0xbf)                         \
    V(i32extend8_s, "i32.extend8_s", 0xc0)                                     \
    V(i32extend16_s, "i32.extend16_s", 0xc1)                                   \
    V(i64extend8_s, "i64.extend8_s", 0xc2)                                     \
    V(i64extend16_s, "i64.extend16_s", 0xc3)                                   \
    V(i64extend32_s, "i64.extend32_s", 0xc4)                                   \
    V(ref_null, "ref.null", 0xd0)                                              \
    V(ref_is_null, "ref.is_null", 0xd1)                                        \
    V(ref_func, "ref.func", 0xd2)                                              \
    V(ref_eq, "ref.eq", 0xd5)                                                  \
    V(multibyte, "multibyte", 0xfc)

#define FOREACH_MULTIBYTE_INSTRUCTION(V)                                       \
    V(i32_trunc_sat_f32_s, "i32.trunc_sat_f32_s", 0x00)                        \
    V(i32_trunc_sat_f32_u, "i32.trunc_sat_f32_u", 0x01)                        \
    V(i32_trunc_sat_f64_s, "i32.trunc_sat_f64_s", 0x02)                        \
    V(i32_trunc_sat_f64_u, "i32.trunc_sat_f64_u", 0x03)                        \
    V(i64_trunc_sat_f32_s, "i64.trunc_sat_f32_s", 0x04)                        \
    V(i64_trunc_sat_f32_u, "i64.trunc_sat_f32_u", 0x05)                        \
    V(i64_trunc_sat_f64_s, "i64.trunc_sat_f64_s", 0x06)                        \
    V(i64_trunc_sat_f64_u, "i64.trunc_sat_f64_u", 0x07)                        \
    V(memory_init, "memory.init", 0x08)                                        \
    V(data_drop, "data.drop", 0x09)                                            \
    V(memory_copy, "memory.copy", 0x0a)                                        \
    V(memory_fill, "memory.fill", 0x0b)                                        \
    V(table_init, "table.init", 0x0c)                                          \
    V(elem_drop, "elem.drop", 0x0d)                                            \
    V(table_copy, "table.copy", 0x0e)                                          \
    V(table_grow, "table.grow", 0x0f)                                          \
    V(table_size, "table.size", 0x10)                                          \
    V(table_fill, "table.fill", 0x11)

enum class Instruction {
#define DEFINE_ENUM(name, str, byte) name = byte,
    FOREACH_INSTRUCTION(DEFINE_ENUM)
#undef DEFINE_ENUM
};

enum class FCInstruction {
#define DEFINE_ENUM(name, str, byte) name = byte,
    FOREACH_MULTIBYTE_INSTRUCTION(DEFINE_ENUM)
#undef DEFINE_ENUM
};

#ifdef WASM_DEBUG
static std::string instructions[] = {
#define DEFINE_NAME(name, str, byte) [byte] = str,
    FOREACH_INSTRUCTION(DEFINE_NAME)
#undef DEFINE_NAME
};

static std::string multibyte_instructions[] = {
#define DEFINE_NAME(name, str, byte) [byte] = str,
    FOREACH_MULTIBYTE_INSTRUCTION(DEFINE_NAME)
#undef DEFINE_NAME
};
#endif

static inline bool is_instruction(uint8_t byte) {
#define DEFINE_EQ(name, str, byte_)                                            \
    if (byte == byte_)                                                         \
        return true;
    FOREACH_INSTRUCTION(DEFINE_EQ)
#undef DEFINE_EQ
    return false;
}
} // namespace mitey