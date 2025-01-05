#include "backend/arm64.hpp"
#include "interfacing.hpp"
#include "module.hpp"
#include "pager/mac.hpp"
#include <iostream>

using namespace mitey;

uint64_t clock_ms() {
    return std::chrono::duration_cast<std::chrono::milliseconds>(
               std::chrono::system_clock::now().time_since_epoch())
        .count();
}

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
    auto mod = Module::compile<Mac, Arm64>(bytes);
    auto end = std::chrono::high_resolution_clock::now();
    printf("Compilation/validation took %fms\n",
           std::chrono::duration<float, std::milli>(end - start).count());

    auto clock_fn = mitey::internalize<clock_ms>();
    runtime::Imports imports{{"env", {{"clock_ms", clock_fn}}}};
    auto instance = mod->instantiate(imports);

    float score = externalize<float()>(
        std::get<runtime::FunctionInfo>(instance->get_exports().at("run")))();

    std::cout << "Score: " << score << std::endl;
}
