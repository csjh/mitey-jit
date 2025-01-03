#pragma once

#include "spec.hpp"
#include <cstdint>
#include <cstring>
#include <memory>

namespace mitey {

class Instance;

namespace runtime {

union WasmValue;
struct WasmMemory;

struct __attribute__((packed)) FunctionType {
    uint16_t params;
    uint16_t results;
    bool has_i32 : 1;
    bool has_i64 : 1;
    bool has_f32 : 1;
    bool has_f64 : 1;
    bool has_funcref : 1;
    bool has_externref : 1;
    uint64_t hash : 64 - 6;

    bool operator==(const FunctionType &other) const {
        return std::memcmp(this, &other, sizeof(FunctionType)) == 0;
    }

    FunctionType(const WasmSignature &sig)
        : params(sig.params.size()), results(sig.results.size()), hash(0) {
        for (valtype param : sig.params) {
            switch (param) {
            case valtype::i32:
                has_i32 = true;
                break;
            case valtype::i64:
                has_i64 = true;
                break;
            case valtype::f32:
                has_f32 = true;
                break;
            case valtype::f64:
                has_f64 = true;
                break;
            case valtype::funcref:
                has_funcref = true;
                break;
            case valtype::externref:
                has_externref = true;
                break;
            case valtype::null:
            case valtype::any:
                error<std::runtime_error>("invalid result type");
            }
            hash *= 16777619;
            hash ^= static_cast<uint64_t>(param);
        }
        hash *= 31;
        for (valtype result : sig.results) {
            switch (result) {
            case valtype::i32:
                has_i32 = true;
                break;
            case valtype::i64:
                has_i64 = true;
                break;
            case valtype::f32:
                has_f32 = true;
                break;
            case valtype::f64:
                has_f64 = true;
                break;
            case valtype::funcref:
                has_funcref = true;
                break;
            case valtype::externref:
                has_externref = true;
                break;
            case valtype::null:
            case valtype::any:
                error<std::runtime_error>("invalid result type");
            }
            hash *= 31;
            hash ^= static_cast<uint64_t>(result);
        }
    }
};
static_assert(sizeof(FunctionType) == sizeof(uint32_t) + sizeof(uint64_t));

using Signature = void(WasmMemory *memory, void **misc, WasmValue *stack,
                       uint64_t tmp1, uint64_t tmp2);

struct FunctionInfo {
    FunctionType type;
    std::shared_ptr<Instance> instance;
    Signature *signature;
};

using Funcref = FunctionInfo *;
using Externref = void *;

union WasmValue {
    int32_t i32;
    uint32_t u32;
    int64_t i64;
    uint64_t u64;
    float f32;
    double f64;
    Funcref funcref;
    Externref externref;

    WasmValue() : u64(0) {}

    WasmValue(int32_t i32) : i32(i32) {}
    WasmValue(uint32_t u32) : u32(u32) {}
    WasmValue(int64_t i64) : i64(i64) {}
    WasmValue(uint64_t u64) : u64(u64) {}
    WasmValue(float f32) : f32(f32) {}
    WasmValue(double f64) : f64(f64) {}
    WasmValue(Funcref funcref) : funcref(funcref) {}
    WasmValue(Externref externref) : externref(externref) {}

    operator int32_t() { return i32; }
    operator uint32_t() { return u32; }
    operator int64_t() { return i64; }
    operator uint64_t() { return u64; }
    operator float() { return f32; }
    operator double() { return f64; }
    operator Funcref() { return funcref; }
    operator Externref() { return externref; }
};

struct ElementSegment {
    valtype type;
    uint32_t size;
    std::unique_ptr<WasmValue[]> elements;
};

struct WasmTable {
    uint32_t current;
    uint32_t maximum;
    WasmValue *elements;

    WasmTable(uint32_t initial, uint32_t maximum)
        : current(initial), maximum(maximum),
          elements(
              static_cast<WasmValue *>(calloc(initial, sizeof(WasmValue)))) {}

    WasmTable() = delete;
    WasmTable(const WasmTable &) = delete;
    WasmTable(WasmTable &&) = delete;
    WasmTable &operator=(const WasmTable &) = delete;
    WasmTable &operator=(WasmTable &&) = delete;

    uint32_t size() { return current; }
    uint32_t max() { return maximum; }
    uint32_t grow(uint32_t delta, WasmValue value);

    WasmValue get(uint32_t idx);
    void set(uint32_t idx, WasmValue value);

    void copy_into(uint32_t dest, uint32_t src, const ElementSegment &segment,
                   uint32_t length);
    void memcpy(WasmTable &dst_table, uint32_t dst, uint32_t src,
                uint32_t length);
    void memset(uint32_t dst, WasmValue value, uint32_t length);
};

struct Segment {
    uint32_t memidx;
    uint32_t size;
    std::unique_ptr<uint8_t[]> data;
    uint8_t *initializer;
};

struct WasmMemory {
    static constexpr uint32_t MAX_PAGES = 65536;
    static constexpr uint32_t PAGE_SIZE = 65536;

    uint32_t current;
    uint32_t maximum;
    // todo: make this be passed around in calling convetion
    // can be updated after memory.grow or unknown calls
    uint8_t *memory;

    WasmMemory(uint32_t initial, uint32_t maximum)
        : current(initial), maximum(std::min(maximum, MAX_PAGES)),
          memory(static_cast<uint8_t *>(calloc(initial, PAGE_SIZE))) {}

    ~WasmMemory() { free(memory); }

    WasmMemory() = delete;
    WasmMemory(const WasmMemory &) = delete;
    WasmMemory(WasmTable &&) = delete;
    WasmMemory &operator=(const WasmMemory &) = delete;
    WasmMemory &operator=(WasmMemory &&) = delete;

    uint32_t size() { return current; }
    uint32_t max() { return maximum; }
    uint32_t grow(uint32_t delta);

    template <typename StackT, typename MemT> WasmValue load(uint32_t addr) {
        if (addr + sizeof(MemT) > current * PAGE_SIZE) {
            trap("out of bounds memory access");
        }
        MemT val;
        std::memcpy(&val, memory + addr, sizeof(val));
        return WasmValue(StackT(val));
    }

    template <typename StackT, typename MemT>
    void store(uint32_t addr, StackT value) {
        if (addr + sizeof(MemT) > current * PAGE_SIZE) {
            trap("out of bounds memory access");
        }
        MemT val = value;
        std::memcpy(memory + addr, &val, sizeof(val));
    }

    void copy_into(uint32_t dest, uint32_t src, const Segment &segment,
                   uint32_t length);
    void memcpy(uint32_t dst, uint32_t src, uint32_t length);
    void memset(uint32_t dst, uint8_t value, uint32_t length);
};

struct WasmGlobal {
    valtype type;
    mut _mut;
    WasmValue value;
};

struct BrTableTarget {
    int32_t lookup_offset;
    int32_t stack_offset;
};
static_assert(sizeof(BrTableTarget) == sizeof(uint64_t));

struct BrInfo {
    uint16_t n_targets; // for br_table
    uint16_t arity;
    int32_t stack_offset; // offset from stack base to copy arity to
};
static_assert(sizeof(BrInfo) == sizeof(uint64_t));

struct CallIndirectInfo {
    uint32_t table_idx;
    FunctionType type;
};
static_assert(sizeof(CallIndirectInfo) == 2 * sizeof(uint64_t));

Signature ifXXconst;
Signature clear_locals;
Signature move_results;
Signature jump;
#define HANDLER(name, str, byte) Signature name;
FOREACH_INSTRUCTION(HANDLER)
FOREACH_MULTIBYTE_INSTRUCTION(HANDLER)
#undef HANDLER

}; // namespace runtime
}; // namespace mitey