#include <cstddef>
#include <cstdint>

namespace mitey {

// places an N-bit immediate Offset bits into a 32-bit word
// ---------------xxxxxxx----------
// ^^ above would be Immediate<7, 15>
template <size_t Bits, size_t Offset> struct Immediate {
    uint32_t *base;

    constexpr Immediate(uint32_t *base) : base(base) {}

    constexpr void operator=(int32_t value) {
        // sign lower int32_t to intBits_t
        uint32_t imm = value;
        imm = imm << (32 - Bits) >> (32 - Bits);

        auto v = *base;
        auto high_len = Offset;
        auto low_len = 32 - high_len - Bits;

        auto low = (v << (high_len + Bits)) >> (high_len + Bits);
        auto high = (v >> (low_len + Bits)) << (low_len + Bits);

        *base = high | (imm << low_len) | low;
    }
};

}; // namespace mitey