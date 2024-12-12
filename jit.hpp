#include <cstdint>
#include <vector>

class JITCompiler {
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

  public:
    std::vector<uint8_t> generateSqrtCode() {
        uint64_t sqrtf_addr = (uint64_t)&sqrtf;

        std::vector<uint8_t> code;
        auto prelude = get_prelude(), postlude = get_postlude(),
             call_sqrtf = call(sqrtf_addr);
        code.insert(code.end(), prelude.begin(), prelude.end());
        code.insert(code.end(), call_sqrtf.begin(), call_sqrtf.end());
        code.insert(code.end(), postlude.begin(), postlude.end());

        return code;
    }
};
