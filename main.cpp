#include "backend/arm64.hpp"
#include "interfacing.hpp"
#include "pager/mac.hpp"
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

    auto instance = mod->instantiate();
    auto &exports = instance->get_exports();

    auto i = 1;
    for (auto f : {
             "fac-rec",
             "fac-rec-named",
             "fac-iter",
             "fac-iter-named",
             "fac-opt",
             "fac-ssa",
         }) {
        auto fn = externalize<uint64_t(uint64_t)>(
            std::get<runtime::FunctionInfo>(exports.at(f)));

        std::cout << fn(i) << std::endl;
        i *= 2;
    }

    return 0;
}
