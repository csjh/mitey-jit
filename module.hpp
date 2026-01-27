#pragma once

#include "./instance.hpp"
#include "pager/shared.hpp"
#include "runtime.hpp"
#include "spec.hpp"
#include <cstddef>
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

struct alignas(128) FunctionShell {
    std::byte *start;
    std::byte *trampoline;

    std::optional<ImportSpecifier> import;

    WasmSignature &type;
    std::vector<valtype> locals;
    bool is_declared;
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

struct Function {
    FunctionShell &fn;
};

struct Block {};

struct Loop {
    std::byte *start;
};

struct If {
    std::byte *else_jump;
};

struct IfElse {};

struct PendingBrTable {
    std::byte *table;
    std::byte *target;
};

struct ControlFlow {
    std::span<valtype> expected;
    std::vector<std::byte *> pending_br;
    std::vector<std::byte *> pending_br_if;
    std::vector<PendingBrTable> pending_br_tables;
    WasmSignature &sig;
    bool polymorphized;
    bool unreachable;
    int32_t stack_offset;
    std::variant<Function, Block, Loop, If, IfElse> construct;
};

class WasmStack {
    static valtype buffer_start[65536];

    valtype *__restrict__ buffer = buffer_start;
    uint64_t polymorphized_and_stack_size;

    constexpr static uint64_t polymorphized_mask = 1ull << 63;
    constexpr static uint64_t stack_size_mask = ~polymorphized_mask;

    template <typename T> auto find_diverging(const T &expected) const;

  public:
    WasmStack(uint64_t initial_sp);

    auto rbegin() const { return std::reverse_iterator(buffer); }
    auto rend() const {
        return std::reverse_iterator(const_cast<valtype *>(buffer_start));
    }

    int64_t sp() { return polymorphized_and_stack_size & stack_size_mask; }

    auto begin() const { return const_cast<valtype *>(buffer_start + 1024); }
    auto end() const { return buffer; }
    auto size() const { return std::distance(begin(), end()); }

    template <typename T> bool check_polymorphic(const T &expected) const;
    template <typename T> bool check(const T &expected) const;

    template <typename T> bool operator==(const T &rhs) const;

    bool polymorphism() const;
    void set_polymorphism(bool p);
    void unpolymorphize();
    void polymorphize();

    void push(valtype ty);
    template <typename T> void push(const T &values);
    void pop(valtype expected_ty);
    template <typename T> void pop_polymorphic(const T &expected);
    template <typename T> void pop(const T &expected);

    bool empty() const;
    bool can_be_anything() const;

    valtype back() const;

    template <size_t pc, size_t rc>
    void apply(std::array<valtype, pc> params, std::array<valtype, rc> results);

    void apply(const WasmSignature &signature);

    void enter_flow(std::span<valtype> expected);
    void leave_flow(std::span<valtype> results);
    void check_br(std::vector<ControlFlow> &control_stack, uint32_t depth);
};

class Module;

template <typename Target>
using CompilationHandler = __attribute__((preserve_none))
std::pair<uint8_t *, std::byte *>(Module &, safe_byte_iterator, FunctionShell &,
                                  WasmStack, std::vector<ControlFlow> &,
                                  std::byte *, Target &);

class Module {
    friend class Instance;

#define V(name, _, byte)                                                       \
    template <typename Target>                                                 \
    friend CompilationHandler<Target> validate_##name;
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
    std::pair<std::uint8_t *, std::byte *>
    validate_and_compile(safe_byte_iterator iter, std::byte *code,
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
        // this is jank but not really sure where to put it
        // Pager and Target should probably be determined by ifdefs
        // instead of actually being passed in
        if (!Instance::initial_stack)
            Instance::initial_stack =
                Pager::allocate(Instance::STACK_SIZE, AllocationKind::Stack);
        // same problem as above but even worse
        runtime::WasmMemory::default_make_memory = Pager::allocate;
        runtime::WasmMemory::default_grow_memory = Pager::grow;
        return mod;
    }

    std::shared_ptr<Instance> instantiate(const runtime::Imports &imports = {});
};

} // namespace mitey

#ifndef WASM_SPECIALIZATION
#include "module-impl.hpp"
#endif
