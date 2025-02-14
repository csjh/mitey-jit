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

// todo: maybe put in callee saved? can bench
constexpr auto memreg = ireg::x0;
constexpr auto miscreg = ireg::x1;

// locals go in callee saved registers, because we don't want to save them
// around every call
// stack scratch can probably go in caller saved registers, because there's
// probably less of them at callsites

// some basics:
// instead of just tracking type, validation stack also tracks:
// - location (register name or stack)
//   - stack offset is implicit, even variables in registers are "on the stack"
//     for easier spills
//   - note register name can be a local
// - whether or not it's a const, if so its value
// - whether or not it's a flag
//   - note there can't be multiple flags in the stack at once, so it has to be
//     tracked (by index) and spilled if another flag comes along before its use

// calling convention:
// - scratch registers are caller saved (duh)
//   - opportunity for optimization here; after all functions are compiled, can
//     know whether or not a function actually clobbers a given register
// - parameters are passed on the stack
//   - this could probably be optimized more; note that stack pointer could be
//     decremented instead of pushing to the stack, because stuff past sp is
//     dead values
// - results are passed in caller saved registers, or on the stack if there are
//   too many

// block handling:
// - things that can be clobbered in a block:
//   - flags
//     - flags will always be clobbered in a block (otherwise it's a block
//     without conditions, which is stupid) so always spill them
//   - scratch registers
//     - put a nop at the start for each scratch reg thing in scope
//     - todo: would it be better to just put 16 nops for everything? prolly not
//   - by the end we know what's clobbered so that can be done anyways
// - things that can't be clobbered in a block:
//   - locals
//   - the stack

} // namespace arm64
} // namespace mitey