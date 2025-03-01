#pragma once

#include <bit>
#include <cstddef>
#include <cstdint>

#define ASSUME(cond)                                                           \
    do {                                                                       \
        if (!(cond))                                                           \
            __builtin_unreachable();                                           \
    } while (0)

namespace mitey {
namespace arm64 {

class reg_lru {
    static constexpr auto Bits = 4;
    static constexpr auto Max = (1 << Bits) - 1;

    uint64_t store = 0xfedcba9876543210;
    static constexpr auto StoreBits = sizeof(store) * 8;

    static constexpr auto LowBitsMask = 0x7777777777777777ull;
    static constexpr auto OnesBitMask = 0x1111111111111111ull;

    // returns the match with a single bit set, in the ones digit of the match
    // adapted from stringzilla
    size_t find(size_t n) {
        ASSUME(n <= Max);

        n *= OnesBitMask;
        auto mask = ~(store ^ n);
        // The match is valid, if every bit within each byte is set. For that,
        // take the bottom 7 bits of each byte, add one to them, and if this
        // sets the top bit to one, then all the 7 bits are ones as well.
        mask = ((mask & LowBitsMask) + OnesBitMask) & ((mask & ~LowBitsMask));
        return mask >> 3;
    }

  public:
    void bump(size_t n) {
        ASSUME(n <= Max);

        auto mask = find(n);
        ASSUME(std::popcount(mask) == 1);

        // elements (pos, 16) remain the same
        // mask-1 because all bottom bits are below the single match bit
        auto bottom_mask = mask - 1;
        // elements [0, pos] are rotated 4 bits to the right
        auto top_mask = ~bottom_mask;

        auto top = (n << (StoreBits - Bits)) | ((store >> Bits) & top_mask);
        auto bottom = store & bottom_mask;

        store = top | bottom;
    }

    size_t claim() {
        size_t last = store & Max;
        // compiler not smart enough to figure out bump(last) is equivalent
        store = std::rotr(store, Bits);
        return last;
    }

    void discard(size_t n) {
        bump(n);
        store = std::rotl(store, Bits);
    }
};

} // namespace arm64
} // namespace mitey
