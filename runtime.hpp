#include <cstdint>
#include <span>
#include <vector>

namespace mitey {

enum class valtype : uint8_t {
    empty = 0x40,

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

union WasmValue;

using Signature = void(uint8_t *memory, WasmValue *stack, WasmValue *locals,
                       void **globals_and_tables, uint64_t tmp1, uint64_t tmp2);

using Funcref = Signature *;
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
    std::vector<WasmValue> elements;
};

struct WasmTable {
    uint32_t current;
    uint32_t maximum;
    WasmValue elements[];

    uint32_t size() { return current; }
    uint32_t max() { return maximum; }
    static uint32_t grow(WasmTable *&thith, uint32_t delta, WasmValue value);

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
    std::span<uint8_t> data;
    uint8_t *initializer;
};

struct WasmMemory {
    static constexpr uint32_t MAX_PAGES = 65536;
    static constexpr uint32_t PAGE_SIZE = 65536;

    uint32_t current;
    uint32_t maximum;
    uint8_t memory[];

    uint32_t size() { return current; }
    uint32_t max() { return maximum; }
    static uint32_t grow(WasmMemory *&thith, uint32_t delta);

    void copy_into(uint32_t dest, uint32_t src, const Segment &segment,
                   uint32_t length);
    void memcpy(uint32_t dst, uint32_t src, uint32_t length);
    void memset(uint32_t dst, uint8_t value, uint32_t length);
};

}; // namespace mitey