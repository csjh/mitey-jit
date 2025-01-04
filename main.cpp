#include "backend/arm64.hpp"
#include "instance.hpp"
#include "pager/mac.hpp"
#include "runtime.hpp"
#include <iostream>

using namespace mitey;

int main(int argc, char **argv) {
    if (argc < 2) {
        printf("Usage: %s <filename>\n", argv[0]);
        return 1;
    }

    auto filename = argv[1];

    auto file = fopen(filename, "rb");
    if (!file) {
        printf("Could not open file %s\n", filename);
        return 1;
    }

    fseek(file, 0, SEEK_END);
    auto length = ftell(file);
    fseek(file, 0, SEEK_SET);

    auto bytes = std::vector<uint8_t>(length);
    fread(bytes.data(), 1, length, file);
    fclose(file);

    auto start = std::chrono::high_resolution_clock::now();
    auto mod = Module::compile<MacExecutable, Arm64>(bytes);
    auto end = std::chrono::high_resolution_clock::now();
    printf("Compilation/validation took %fms\n",
           std::chrono::duration<float, std::milli>(end - start).count());

    auto fac = reinterpret_cast<runtime::Signature *>(mod->functions[0].start);
    auto fac_bytes = mod->functions[0].start;

    runtime::WasmValue s[65536] = {(double)170.0};
    start = std::chrono::high_resolution_clock::now();
    fac(nullptr, s + 1, nullptr, 0, 0);
    end = std::chrono::high_resolution_clock::now();
    printf("Execution took %fms\n",
           std::chrono::duration<float, std::milli>(end - start).count());

    auto score = s[0].f64;
    printf("Score: %f\n", score);

    return 0;
}
