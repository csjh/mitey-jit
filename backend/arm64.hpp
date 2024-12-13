#include "target.hpp"
#include <cstring>
#include <libkern/OSCacheControl.h>
#include <sys/mman.h>
#include <unistd.h>
#include <vector>

class Arm64 : public Target {
  public:
    size_t prelude_size() override { return get_prelude().size(); }
    std::vector<uint8_t> get_prelude() override {
        auto arm = std::vector<uint32_t>{
            0xa9bf7bfd, // stp     x29, x30, [sp, #-0x10]!
            0x910003fd  // mov     x29, sp
        };

        return u32_to_u8(arm);
    }

    size_t postlude_size() override { return get_postlude().size(); }
    std::vector<uint8_t> get_postlude() override {
        auto arm = std::vector<uint32_t>{
            0xa8c17bfd, // ldp     x29, x30, [sp], #0x10
            0xd65f03c0  // ret
        };

        return u32_to_u8(arm);
    }

    size_t call_size() override { return call(0).size(); }
    std::vector<uint8_t> call(uint64_t addr) override {
        constexpr uint8_t x6 = 6;
        auto instructions = mov64(addr, x6);
        instructions.push_back((0b1101011000111111000000u << 10) | (x6 << 5));
        return u32_to_u8(instructions);
    }

    size_t temp1_size() override { return set_temp1(0).size(); }
    std::vector<uint8_t> set_temp1(uint64_t value) override {
        constexpr uint8_t x3 = 3;
        auto instructions = mov64(value, x3);
        return u32_to_u8(instructions);
    }

    size_t temp2_size() override { return set_temp2(0).size(); }
    std::vector<uint8_t> set_temp2(uint64_t value) override {
        constexpr uint8_t x4 = 4;
        auto instructions = mov64(value, x4);
        return u32_to_u8(instructions);
    }

  private:
    std::vector<uint8_t> u32_to_u8(std::vector<uint32_t> u32) {
        return std::vector<uint8_t>((uint8_t *)u32.data(),
                                    (uint8_t *)(u32.data() + u32.size()));
    }

    std::vector<uint32_t> mov64(uint64_t value, uint8_t reg) {
        // todo: test pc-relative ldr instead (smaller, maybe more perf?)
        return {mov16((value >> 0) & 0xffff, 0, reg),
                mov16((value >> 16) & 0xffff, 1, reg),
                mov16((value >> 32) & 0xffff, 2, reg),
                mov16((value >> 48) & 0xffff, 3, reg)};
    }

    uint32_t mov16(uint16_t imm, uint8_t shift, uint8_t reg) {
        return (0b111100101 << 23) | (shift << 21) | (imm << 5) | reg;
    }
};
