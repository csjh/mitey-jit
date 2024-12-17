#pragma once

#include "../runtime.hpp"
#include <cstdint>
#include <vector>

namespace mitey {
struct Target {
    // wasm function prelude/postlude
    virtual std::vector<uint8_t> get_prelude() = 0;
    virtual std::vector<uint8_t> get_postlude() = 0;

    // (non-wasm?) function call (ideally wasm calls would just be relative jmp)
    virtual std::vector<uint8_t> call(mitey::Signature *) = 0;

    // temp registers, used for passing compile-time constants (i.e. lookup
    // table address)
    virtual std::vector<uint8_t> set_temp1(uint64_t value) = 0;
    virtual std::vector<uint8_t> set_temp2(uint64_t value) = 0;

    // used for calculating memory offsets and such
    virtual size_t prelude_size() = 0;
    virtual size_t postlude_size() = 0;
    virtual size_t call_size() = 0;
    virtual size_t temp1_size() = 0;
    virtual size_t temp2_size() = 0;

    virtual ~Target() = default;
};
} // namespace mitey