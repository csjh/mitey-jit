#pragma once

#include <cstdint>
#include <cstring>

namespace mitey {

// places an N-bit immediate Offset bits into a 32-bit word
// ---------------xxxxxxx----------
// ^^ above would be Immediate<7, 15>
struct Immediate {
    void *base;
    uint16_t bits;
    uint16_t offset;
    uint16_t align;

    void set(void *to) {
        auto diff = static_cast<uint8_t *>(to) - static_cast<uint8_t *>(base);
        auto value = diff / align;

        // sign lower int32_t to intBits_t
        auto imm = static_cast<uint32_t>(value);
        imm = imm << (32 - bits) >> (32 - bits);

        uint32_t v;
        std::memcpy(&v, base, sizeof(v));

        auto high_len = offset;
        auto low_len = 32 - high_len - bits;

        auto low = (v << (high_len + bits)) >> (high_len + bits);
        auto high = (v >> (low_len + bits)) << (low_len + bits);

        uint32_t result = high | (imm << low_len) | low;
        std::memcpy(base, &result, sizeof(result));
    }
};

}; // namespace mitey