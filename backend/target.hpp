#include <cstdint>
#include <vector>

struct Target {
    // wasm function prelude/postlude
    virtual constexpr std::vector<uint8_t> get_prelude() = 0;
    virtual constexpr std::vector<uint8_t> get_postlude() = 0;

    // (non-wasm?) function call (ideally wasm calls would just be relative jmp)
    virtual constexpr std::vector<uint8_t> call(uint64_t addr) = 0;

    // temp registers, used for passing compile-time constants (i.e. lookup
    // table address)
    virtual constexpr std::vector<uint8_t> set_temp1(uint64_t value) = 0;
    virtual constexpr std::vector<uint8_t> set_temp2(uint64_t value) = 0;

    // used for calculating memory offsets and such
    virtual constexpr size_t prelude_size() = 0;
    virtual constexpr size_t postlude_size() = 0;
    virtual constexpr size_t call_size() = 0;
    virtual constexpr size_t temp1_size() = 0;
    virtual constexpr size_t temp2_size() = 0;

    virtual ~Target() = default;
};
