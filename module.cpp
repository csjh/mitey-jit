#include "module.hpp"
#include "instance.hpp"
#include "spec.hpp"
#include <limits>

#ifdef WASM_DEBUG
#include <iostream>
#endif

namespace mitey {

safe_byte_iterator::safe_byte_iterator(uint8_t *ptr, size_t length)
    : iter(ptr), end(ptr + length) {}

safe_byte_iterator::safe_byte_iterator(uint8_t *ptr, uint8_t *end)
    : iter(ptr), end(end) {}

uint8_t safe_byte_iterator::operator*() const {
    if (iter >= end) {
        error<malformed_error>("unexpected end");
    }
    return *iter;
}

uint8_t safe_byte_iterator::operator[](ssize_t n) const {
    if (iter + n >= end) {
        error<malformed_error>("unexpected end");
    }
    return iter[n];
}

safe_byte_iterator &safe_byte_iterator::operator++() {
    ++iter;
    return *this;
}

safe_byte_iterator safe_byte_iterator::operator++(int) {
    return safe_byte_iterator(iter++, end);
}

safe_byte_iterator safe_byte_iterator::operator+(size_t n) const {
    return safe_byte_iterator(iter + n, end);
}

safe_byte_iterator &safe_byte_iterator::operator+=(size_t n) {
    iter += n;
    return *this;
}

ptrdiff_t safe_byte_iterator::operator-(safe_byte_iterator other) const {
    return iter - other.iter;
}

ptrdiff_t safe_byte_iterator::operator-(const uint8_t *other) const {
    return iter - other;
}

bool safe_byte_iterator::operator<(safe_byte_iterator other) const {
    return iter < other.iter;
}

uint8_t *safe_byte_iterator::get_with_at_least(size_t n) const {
    if (!has_n_left(n)) {
        error<malformed_error>("length out of bounds");
    }
    return iter;
}

bool safe_byte_iterator::empty() const { return iter == end; }

bool safe_byte_iterator::has_n_left(size_t n) const { return iter + n <= end; }

std::tuple<uint32_t, uint32_t> get_limits(safe_byte_iterator &iter,
                                          uint32_t upper_limit) {
    auto flags = safe_read_leb128<uint32_t, 1>(iter);
    auto initial = safe_read_leb128<uint32_t>(iter);
    auto max = flags == 1 ? safe_read_leb128<uint32_t>(iter) : upper_limit;
    return {initial, max};
}

std::tuple<uint32_t, uint32_t> get_memory_limits(safe_byte_iterator &iter) {
    auto [initial, max] = get_limits(iter, Module::MAX_PAGES);
    if (initial > Module::MAX_PAGES || max > Module::MAX_PAGES) {
        error<validation_error>(
            "memory size must be at most 65536 pages (4GiB)");
    }
    if (max < initial) {
        error<validation_error>(
            "size minimum must not be greater than maximum");
    }
    return {initial, max};
}

std::tuple<uint32_t, uint32_t> get_table_limits(safe_byte_iterator &iter) {
    auto [initial, max] =
        get_limits(iter, std::numeric_limits<uint32_t>::max());
    if (max < initial) {
        error<validation_error>(
            "size minimum must not be greater than maximum");
    }
    return {initial, max};
}

WasmSignature &read_blocktype(std::vector<WasmSignature> &types,
                              safe_byte_iterator &iter) {
    constexpr uint8_t empty_type = 0x40;
    static WasmSignature singles[] = {
        [0x40] = {{}, {}},
        [0x7f] = {{}, {valtype::i32}},
        [0x7e] = {{}, {valtype::i64}},
        [0x7d] = {{}, {valtype::f32}},
        [0x7c] = {{}, {valtype::f64}},
        [0x70] = {{}, {valtype::funcref}},
        [0x6f] = {{}, {valtype::externref}},
    };

    uint8_t byte = *iter;
    if (byte == empty_type || is_valtype(byte)) {
        ++iter;
        return singles[byte];
    } else {
        int64_t n = safe_read_sleb128<int64_t, 33>(iter);
        return types[n];
    }
}

void Module::validate_const(safe_byte_iterator &iter, valtype expected) {
    std::vector<valtype> stack_types;

#define OP(ty, op)                                                             \
    {                                                                          \
        if (stack_types.size() < 2) {                                          \
            error<validation_error>("type mismatch");                          \
        }                                                                      \
        if (stack_types[stack_types.size() - 1] != stack_types.back()) {       \
            error<validation_error>("type mismatch");                          \
        }                                                                      \
        if (stack_types.back() != valtype::ty) {                               \
            error<validation_error>("type mismatch");                          \
        }                                                                      \
        stack_types.pop_back();                                                \
        break;                                                                 \
    }
#define I32_OP(op) OP(i32, op)
#define I64_OP(op) OP(i64, op)

    while (1) {
        auto byte = *iter++;

        using enum Instruction;
        if (static_cast<Instruction>(byte) == end) {
            break;
        }
        switch (static_cast<Instruction>(byte)) {
        case i32const:
            safe_read_sleb128<int32_t>(iter);
            stack_types.push_back(valtype::i32);
            break;
        case i64const:
            safe_read_sleb128<int64_t>(iter);
            stack_types.push_back(valtype::i64);
            break;
        case f32const: {
            iter += sizeof(float);
            stack_types.push_back(valtype::f32);
            break;
        }
        case f64const: {
            iter += sizeof(double);
            stack_types.push_back(valtype::f64);
            break;
        }
        case globalget: {
            auto global_idx = safe_read_leb128<uint32_t>(iter);
            if (global_idx >= globals.size() || !globals[global_idx].import) {
                error<validation_error>("unknown global");
            }
            if (globals[global_idx].mutability != mut::const_) {
                error<validation_error>("constant expression required");
            }
            stack_types.push_back(globals[global_idx].type);
            break;
        }
        case i32add:
            I32_OP(+);
        case i32sub:
            I32_OP(-);
        case i32mul:
            I32_OP(*);
        case i64add:
            I64_OP(+);
        case i64sub:
            I64_OP(-);
        case i64mul:
            I64_OP(*);
        case ref_null: {
            auto reftype = safe_read_leb128<uint32_t>(iter);
            if (!is_reftype(reftype)) {
                error<validation_error>("type mismatch");
            }
            stack_types.push_back(static_cast<valtype>(reftype));
            break;
        }
        case ref_func: {
            auto func_idx = safe_read_leb128<uint32_t>(iter);
            if (func_idx >= functions.size()) {
                error<validation_error>("unknown function");
            }
            // implicit declaration
            functions[func_idx].is_declared = true;
            stack_types.push_back(valtype::funcref);
            break;
        }
        default:
            if (is_instruction(byte)) {
                error<validation_error>("constant expression required");
            } else {
                error<malformed_error>("illegal opcode");
            }
        }
    }

#undef OP
#undef I32_OP
#undef I64_OP

    if (stack_types.size() != 1 || stack_types[0] != expected) {
        error<validation_error>("type mismatch");
    }
}

std::shared_ptr<Instance> Module::instantiate(const runtime::Imports &imports) {
    auto instance = std::shared_ptr<Instance>(new Instance(this->self.lock()));
    instance->self = instance;
    instance->initialize(imports);
    return instance;
}

Module::Module() : executable(nullptr, [](auto) {}), memory{} {}

WasmStack::WasmStack() : buffer(buffer_start) {
    std::fill_n(buffer, 1024, valtype::null);
    buffer += 1024;
}

bool WasmStack::polymorphism() const { return polymorphized; }

void WasmStack::set_polymorphism(bool p) { polymorphized = p; }

void WasmStack::unpolymorphize() { set_polymorphism(false); }

void WasmStack::polymorphize() {
    set_polymorphism(true);
    buffer = std::find(rbegin(), rend(), valtype::null).base();
}

void WasmStack::push(valtype ty) { push(std::array{ty}); }

void WasmStack::pop(valtype expected_ty) { pop(std::array{expected_ty}); }

bool WasmStack::empty() const {
    return !polymorphized && *rbegin() == valtype::null;
}

bool WasmStack::can_be_anything() const {
    return polymorphized && *rbegin() == valtype::null;
}

valtype WasmStack::back() const {
    if (auto b = *rbegin(); b != valtype::null)
        return b;
    if (polymorphized) {
        return valtype::any;
    } else {
        error<validation_error>("type mismatch");
    }
}

void WasmStack::apply(const WasmSignature &signature) {
    pop(signature.params);
    push(signature.results);
}

void WasmStack::enter_flow(const valtype_vector &expected) {
    pop(expected);
    push(valtype::null);
    push(expected);
}

void WasmStack::check_br(std::vector<ControlFlow> &control_stack,
                         uint32_t depth) {
    ensure(depth < control_stack.size(), "unknown label");
    auto &target = control_stack[control_stack.size() - depth - 1];
    pop(target.expected);
    push(target.expected);
}

} // namespace mitey
