#include <cstdint>
#include <memory>
#include <stdexcept>

namespace mitey {
[[noreturn]] static inline void trap(const char *msg) {
    throw std::runtime_error(msg);
}

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
struct WasmMemory;

using Signature = void(WasmMemory *memory, WasmValue *stack,
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

}; // namespace mitey