#include "module.hpp"
#include "runtime.hpp"
#include <cstdint>
#include <memory>

namespace mitey {

class Instance {
    friend class Module;
    friend runtime::Signature runtime::call_indirect;

    static constexpr uint32_t STACK_SIZE =
        (5 * 1024 * 1024) / sizeof(runtime::WasmValue); // 5mb
    static constexpr uint32_t MAX_DEPTH = 1000;

    Instance(const Instance &) = delete;
    Instance &operator=(const Instance &) = delete;
    Instance(Instance &&) = delete;
    Instance &operator=(Instance &&) = delete;

    std::shared_ptr<Module> module;
    std::weak_ptr<Instance> self;
    std::unique_ptr<void *[]> misc;

    // WebAssembly.Memory
    std::shared_ptr<runtime::WasmMemory> memory;
    // internal stack
    std::unique_ptr<runtime::WasmValue[]> initial_stack;
    // functions
    std::vector<runtime::FunctionInfo> functions;
    // value of globals
    std::vector<std::shared_ptr<runtime::WasmGlobal>> globals;
    // maps element indices to the element initializers
    std::vector<runtime::ElementSegment> elements;
    // tables
    std::vector<std::shared_ptr<runtime::WasmTable>> tables;
    // exports from export section
    Exports exports;

    runtime::WasmValue interpret_const_inplace(uint8_t *iter) {
        return interpret_const(iter);
    }
    runtime::WasmValue interpret_const(uint8_t *&iter);

    Instance(std::shared_ptr<Module> module);

    void initialize(const Imports &imports);

  public:
    const Exports &get_exports() { return exports; }
};

} // namespace mitey