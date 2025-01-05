#pragma once

#include "./pager/shared.hpp"
#include "./runtime.hpp"
#include <cstdint>
#include <functional>
#include <memory>

namespace mitey {

class Module;

class Instance {
    friend class Module;
    friend runtime::Signature runtime::call_indirect;

    template <typename FunctionType>
    friend std::function<FunctionType>
    externalize(const runtime::FunctionInfo &fn);

    friend inline std::function<std::vector<runtime::WasmValue>(
        const std::vector<runtime::WasmValue> &)>
    externalize(const runtime::FunctionInfo &fn);

    Instance(const Instance &) = delete;
    Instance &operator=(const Instance &) = delete;
    Instance(Instance &&) = delete;
    Instance &operator=(Instance &&) = delete;

    std::shared_ptr<Module> module;
    std::weak_ptr<Instance> self;
    std::unique_ptr<void *[]> misc;

    // WebAssembly.Memory
    std::shared_ptr<runtime::WasmMemory> memory;
    // functions
    std::vector<runtime::FunctionInfo> functions;
    // value of globals
    std::vector<std::shared_ptr<runtime::WasmGlobal>> globals;
    // maps element indices to the element initializers
    std::vector<runtime::ElementSegment> elements;
    // tables
    std::vector<std::shared_ptr<runtime::WasmTable>> tables;
    // exports from export section
    runtime::Exports exports;

    runtime::WasmValue interpret_const_inplace(uint8_t *iter) {
        return interpret_const(iter);
    }
    runtime::WasmValue interpret_const(uint8_t *&iter);

    Instance(std::shared_ptr<Module> module);

    void initialize(const runtime::Imports &imports);

  public:
    static constexpr uint32_t STACK_SIZE = 5 * 1024 * 1024; // 5mb
    static Allocation initial_stack;

    const runtime::Exports &get_exports() { return exports; }
};

} // namespace mitey