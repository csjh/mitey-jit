#include "backend/arm64.hpp"
#include "module.hpp"
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

    auto addTwo =
        reinterpret_cast<runtime::Signature *>(mod->functions[0].start);

    runtime::WasmValue s[16] = {10u, 35u};
    addTwo(nullptr, s + 2, nullptr, 0, 0);
    auto jitter = s[0].u32;
    std::cout << "addTwo result: " << jitter << std::endl;
    std::cout << "stack: ";
    for (int i = 0; i < 16; i++) {
        std::cout << s[i].u32 << " ";
    }
    std::cout << std::endl;

    return 0;
}
