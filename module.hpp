#pragma once

#include "pager/executable.hpp"
#include "runtime.hpp"
#include "spec.hpp"
#include <memory>
#include <optional>
#include <span>
#include <vector>

namespace mitey {

class safe_byte_iterator {
    uint8_t *iter;
    uint8_t *end;

  public:
    safe_byte_iterator(uint8_t *ptr, size_t length);
    safe_byte_iterator(uint8_t *ptr, uint8_t *end);

    uint8_t operator*() const;
    uint8_t operator[](ssize_t n) const;
    safe_byte_iterator &operator++();
    safe_byte_iterator operator++(int);
    safe_byte_iterator operator+(size_t n) const;
    safe_byte_iterator &operator+=(size_t n);
    ptrdiff_t operator-(safe_byte_iterator other) const;
    ptrdiff_t operator-(const uint8_t *other) const;
    bool operator<(safe_byte_iterator other) const;
    uint8_t *get_with_at_least(size_t n) const;
    bool empty() const;
    bool has_n_left(size_t n) const;

    uint8_t *unsafe_ptr() const { return iter; }
};

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

class WasmStack {
    bool polymorphized = false;
    valtype buffer_start[65536];
    valtype *buffer;
    uint32_t stack_size = 0;

    auto rbegin() const { return std::reverse_iterator(buffer); }
    auto rend() const {
        return std::reverse_iterator(const_cast<valtype *>(buffer_start));
    }

    template <typename T> auto find_diverging(const T &expected) const;

  public:
    WasmStack();

    auto sp() { return stack_size; }

    auto begin() const { return const_cast<valtype *>(buffer_start + 1024); }
    auto end() const { return buffer; }
    auto size() const { return std::distance(begin(), end()); }

    template <typename T> bool check(const T &expected) const;

    template <typename T> bool operator==(const T &rhs) const;

    bool polymorphism() const;
    void set_polymorphism(bool p);
    void unpolymorphize();
    void polymorphize();

    void push(valtype ty);
    template <typename T> void push(const T &values);
    void pop(valtype expected_ty);
    template <typename T> void pop(const T &expected);

    bool empty() const;
    bool can_be_anything() const;

    valtype back() const;

    template <size_t pc, size_t rc>
    void apply(std::array<valtype, pc> params, std::array<valtype, rc> results);

    void apply(const WasmSignature &signature);
    void enter_flow(const std::vector<valtype> &expected);
    void check_br(std::vector<ControlFlow> &control_stack, uint32_t depth);
};

class Module;

using CompilationHandler = uint8_t *(Module &, safe_byte_iterator &,
                                     FunctionShell &, WasmStack &,
                                     std::vector<ControlFlow> &, uint8_t *);

class Module {
    template <typename Pager, typename Target> friend class JIT;

#define V(name, _, byte)                                                       \
    template <typename Target> friend CompilationHandler validate_##name;
    FOREACH_INSTRUCTION(V)
    FOREACH_MULTIBYTE_INSTRUCTION(V)
#undef V

    std::weak_ptr<Module> self;

    Allocation executable;

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
    std::vector<runtime::Segment> data_segments;

    template <typename Pager, typename Target>
    uint8_t *validate_and_compile(safe_byte_iterator &iter, uint8_t *code,
                                  FunctionShell &fn);

    void validate_const(safe_byte_iterator &iter, valtype expected);

    Module();

    template <typename Pager, typename Target>
    void initialize(std::span<uint8_t> bytes);

  public:
    static constexpr uint32_t MAX_PAGES = 65536;
    static constexpr uint32_t MAX_LOCALS = 50000;

    template <typename Pager, typename Target>
    static std::shared_ptr<Module> compile(std::span<uint8_t> bytes) {
        auto mod = std::shared_ptr<Module>(new Module());
        mod->self = mod;
        mod->initialize<Pager, Target>(bytes);
        return mod;
    }

    // std::shared_ptr<Instance> instantiate(const Imports &imports = {});
};

} // namespace mitey

#include "module-impl.hpp"
