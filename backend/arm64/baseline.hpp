#include <array>
#include <cstdint>

namespace mitey {
namespace arm64 {

// clang-format off
enum class ireg {
    x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15,
    x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, /* x29, x30, x31, */
};

enum class freg {
    d0, d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, d14, d15,
    d16, d17, d18, d19, d20, d21, d22, d23, d24, d25, d26, d27, d28, d29, d30, d31,
};

constexpr auto callee_saved = std::to_array({
    ireg::x19, ireg::x20, ireg::x21, ireg::x22, ireg::x23,
    ireg::x24, ireg::x25, ireg::x26, ireg::x27, ireg::x28});

constexpr auto caller_saved = std::to_array({
    ireg::x0, ireg::x1, ireg::x2, ireg::x3, ireg::x4, ireg::x5, ireg::x6, ireg::x7,
    ireg::x8, ireg::x9, ireg::x10, ireg::x11, ireg::x12, ireg::x13, ireg::x14, ireg::x15,
    ireg::x16, ireg::x17, ireg::x18});
// clang-format on

// locals go in callee saved registers, because we don't want to save them
// around every call
// stack scratch can probably go in caller saved registers, because there's
// probably less of them at callsites

} // namespace arm64
} // namespace mitey