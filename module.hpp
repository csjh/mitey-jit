#pragma once

#include "pager/executable.hpp"
#include "runtime.hpp"
#include "spec.hpp"
#include <memory>
#include <optional>
#include <span>
#include <vector>

namespace mitey {

enum class ImExDesc {
    func,
    table,
    mem,
    global,
};
static inline bool is_imexdesc(uint8_t byte) {
    return byte == static_cast<uint8_t>(ImExDesc::func) ||
           byte == static_cast<uint8_t>(ImExDesc::table) ||
           byte == static_cast<uint8_t>(ImExDesc::mem) ||
           byte == static_cast<uint8_t>(ImExDesc::global);
}

using ImportSpecifier = std::pair<std::string, std::string>;

struct FunctionShell {
    uint8_t *start;
    WasmSignature type;
    std::vector<valtype> locals;
    std::optional<ImportSpecifier> import;
    bool is_declared = false;
};

struct TableShell {
    uint32_t min;
    uint32_t max;
    valtype type;
    std::optional<ImportSpecifier> import;
};

struct MemoryShell {
    uint32_t min;
    uint32_t max;
    bool exists;
    std::optional<ImportSpecifier> import;
};

struct GlobalShell {
    valtype type;
    mut mutability;
    uint8_t *initializer;
    std::optional<ImportSpecifier> import;
};

struct ExportShell {
    ImExDesc desc;
    uint32_t idx;
};

struct ElementShell {
    valtype type;
};

struct Function {};

struct Block {
    uint8_t *block_start;
};

struct Loop {};

struct If {
    uint8_t *if_start;
};

struct IfElse {
    uint8_t *if_start;
    uint8_t *else_start;
};

struct ControlFlow {
    std::vector<valtype> &expected;
    WasmSignature &sig;
    bool polymorphized;
    std::variant<Function, Block, Loop, If, IfElse> construct;
};

class safe_byte_iterator;
class Module;
class WasmStack;

using ValidationHandler = void(Module &, safe_byte_iterator &, FunctionShell &,
                               WasmStack &, std::vector<ControlFlow> &);

class Module {
    template <typename Pager, typename Target> friend class JIT;

#define V(name, _, byte) friend ValidationHandler validate_##name;
    FOREACH_INSTRUCTION(V)
    FOREACH_MULTIBYTE_INSTRUCTION(V)
#undef V

    std::weak_ptr<Module> self;

    Allocation executable;
    std::unique_ptr<uint32_t[]> function_offsets;

    std::vector<WasmSignature> types;
    std::unordered_map<std::string, std::unordered_map<std::string, ImExDesc>>
        imports;
    std::vector<TableShell> tables;
    MemoryShell memory;
    std::vector<GlobalShell> globals;
    std::unordered_map<std::string, ExportShell> exports;
    uint32_t start;
    uint8_t *element_start;
    std::vector<ElementShell> elements;
    std::vector<FunctionShell> functions;
    uint32_t n_data;
    std::vector<Segment> data_segments;

    // todo: maybe split this into another class
    FunctionShell current_fn;
    std::vector<std::vector<valtype>> control_stack;

    void validate(safe_byte_iterator &iter, FunctionShell &fn);

    void validate(safe_byte_iterator &iter, const WasmSignature &signature,
                  bool is_func = false);

    void validate_const(safe_byte_iterator &iter, valtype expected);

    Module();

    void initialize(std::span<uint8_t> bytes);

  public:
    static constexpr uint32_t MAX_PAGES = 65536;
    static constexpr uint32_t MAX_LOCALS = 50000;

    template <typename Pager, typename Target>
    static std::shared_ptr<Module> compile(std::span<uint8_t> bytes) {
        auto mod = std::shared_ptr<Module>(new Module());
        mod->self = mod;
        mod->initialize(bytes);
        return mod;
    }

    // std::shared_ptr<Instance> instantiate(const Imports &imports = {});
};

} // namespace mitey
