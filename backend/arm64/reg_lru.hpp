#pragma once

#include <cassert>
#include <cstdint>

#define ASSUME(cond)                                                           \
    do {                                                                       \
        if (!(cond))                                                           \
            __builtin_unreachable();                                           \
    } while (0)

namespace mitey {
namespace arm64 {

template <uint8_t N> class reg_lru {
    static constexpr uint8_t dummy = 255;

    uint8_t prev[N];
    uint8_t next[N];
    uint8_t head;
    uint8_t tail;

    uint8_t current_temporary;

  public:
    reg_lru() {
        for (uint8_t i = 0; i < N; i++) {
            prev[i] = (i == 0) ? dummy : i - 1;
            next[i] = (i == N - 1) ? dummy : i + 1;
        }
        head = 0;
        tail = N - 1;
    }

    void claim(uint8_t n) {
        assert(n < N);

        if (n == tail) {
            return;
        }

        if (prev[n] != dummy) {
            next[prev[n]] = next[n];
        }
        if (next[n] != dummy) {
            prev[next[n]] = prev[n];
        }
        if (n == head) {
            head = next[n];
        }

        next[tail] = n;
        prev[n] = tail;
        next[n] = dummy;
        tail = n;
    }

    void surrender(uint8_t n) {
        assert(n < N);

        if (n == head) {
            return;
        }

        if (prev[n] != dummy) {
            next[prev[n]] = next[n];
        }
        if (next[n] != dummy) {
            prev[next[n]] = prev[n];
        }
        if (n == tail) {
            tail = prev[n];
            next[tail] = dummy;
        }

        prev[head] = n;
        next[n] = head;
        prev[n] = dummy;
        head = n;
    }

    void reset_temporaries() { current_temporary = head; }
    // takes the least recently used non-claimed register
    uint8_t temporary() {
        auto ret = current_temporary;
        current_temporary = next[ret];
        return ret;
    }
    void untemporary(uint8_t n) {
        if (n == prev[current_temporary])
            current_temporary = n;
    }
    // only allows one result
    uint8_t back() { return head; }
};

} // namespace arm64
} // namespace mitey
