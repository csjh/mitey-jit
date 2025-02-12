#pragma once

#include "module.hpp"
#include "runtime.hpp"
#include "spec.hpp"
#include <numeric>

#ifdef WASM_DEBUG
#include <iostream>

extern std::vector<std::string> names;
#endif

namespace mitey {

std::tuple<uint32_t, uint32_t> get_memory_limits(safe_byte_iterator &iter);
std::tuple<uint32_t, uint32_t> get_table_limits(safe_byte_iterator &iter);

WasmSignature &read_blocktype(std::vector<WasmSignature> &types,
                              safe_byte_iterator &iter);

static inline void ensure(bool condition, const char *msg) {
    if (!condition) [[unlikely]] {
        error<validation_error>(msg);
    }
}

template <typename T> constexpr auto arity = arity<decltype(&T::operator())>;

template <typename R, typename... Args>
constexpr auto arity<R(Args...)> = sizeof...(Args);

template <typename R, typename... Args>
constexpr auto arity<R (*)(Args...)> = arity<R(Args...)>;

template <typename C, typename R, typename... Args>
constexpr auto arity<R (C::*)(Args...)> = arity<R(Args...)>;

template <typename C, typename R, typename... Args>
constexpr auto arity<R (C::*)(Args...) const> = arity<R(Args...)>;

template <typename Pager, typename Target>
void Module::initialize(std::span<uint8_t> bytes) {
    if (bytes.size() < 4) {
        error<malformed_error>("unexpected end");
    }

    auto iter = safe_byte_iterator(bytes.data(), bytes.size());

    if (std::memcmp(iter.get_with_at_least(4), "\0asm", 4) != 0) {
        error<malformed_error>("magic header not detected");
    }
    iter += 4;

    if (bytes.size() < 8) {
        error<malformed_error>("unexpected end");
    }

    uint32_t version;
    std::memcpy(&version, iter.get_with_at_least(4), 4);
    if (version != 1) {
        error<malformed_error>("unknown binary version");
    }
    iter += sizeof(uint32_t);

    auto skip_custom_section = [&]() {
        while (!iter.empty() && *iter == 0) [[unlikely]] {
            ++iter;
            auto section_length = safe_read_leb128<uint32_t>(iter);
            if (!iter.has_n_left(section_length)) {
                error<malformed_error>("length out of bounds");
            }

            auto start = iter;

            auto name_length = safe_read_leb128<uint32_t>(iter);
            if (!is_valid_utf8(iter.get_with_at_least(name_length),
                               name_length)) {
                error<malformed_error>("malformed UTF-8 encoding");
            }

            if (start + section_length < iter) {
                error<malformed_error>("unexpected end");
            }

#ifdef WASM_DEBUG
            // technically this should strictly be after the data section
            // but idc
            if (name_length == 4 &&
                std::memcmp(iter.get_with_at_least(4), "name", 4) == 0) {

                iter += 4;
                // module name subsection
                if (*iter == 0) {
                    iter++;
                    // skip
                    iter += safe_read_leb128<uint32_t>(iter);
                }
                // function name subsection
                if (*iter == 1) {
                    iter++;
                    auto subsection_size = safe_read_leb128<uint32_t>(iter);
                    auto n_names = safe_read_leb128<uint32_t>(iter);

                    names.reserve(n_names);
                    for (uint32_t i = 0; i < n_names; ++i) {
                        auto fn_idx = safe_read_leb128<uint32_t>(iter);
                        auto name_len = safe_read_leb128<uint32_t>(iter);
                        names.push_back(
                            std::string(reinterpret_cast<char *>(
                                            iter.get_with_at_least(name_len)),
                                        name_len));
                        iter += name_len;
                    }
                }
                // local name subsection
                if (*iter == 2) {
                    iter++;
                    // ignore
                }
            }
#endif

            iter = start + section_length;
        }
    };

    auto section = [&](
                       uint32_t id, auto body,
                       std::function<void()> else_ = [] {}) {
        static_assert(arity<decltype(body)> <= 1);

        if (!iter.empty() && *iter == id) {
            ++iter;
            auto section_length = safe_read_leb128<uint32_t>(iter);
            if (!iter.has_n_left(section_length)) {
                error<malformed_error>("length out of bounds");
            }
            auto section_start = iter;

            if constexpr (arity<decltype(body)> == 1) {
                body(section_length);
            } else {
                body();
            }

            if (iter - section_start != section_length) {
                error<malformed_error>("section size mismatch");
            }

            // todo: remove this when validation separates
            if (!iter.empty() && *iter == id) {
                error<malformed_error>("unexpected content after last section");
            }
        } else if (!iter.empty() && *iter > 12) {
            error<malformed_error>("malformed section id");
        } else {
            else_();
        }
    };

    skip_custom_section();

    // type section
    section(1, [&] {
        auto n_types = safe_read_leb128<uint32_t>(iter);

        types.reserve(n_types);

        for (uint32_t i = 0; i < n_types; ++i) {
            if (iter.empty()) {
                error<malformed_error>("unexpected end of section or function");
            }

            if (*iter != 0x60) {
                error<malformed_error>("integer representation too long");
                // error<validation_error>("invalid function type");
            }
            ++iter;

            auto fn = WasmSignature({}, {});

            auto n_params = safe_read_leb128<uint32_t>(iter);
            fn.params.reserve(n_params);
            for (uint32_t j = 0; j < n_params; ++j) {
                if (!is_valtype(iter[j])) {
                    error<validation_error>("invalid parameter type");
                }
                fn.params.push_back(static_cast<valtype>(iter[j]));
            }
            iter += n_params;

            auto n_results = safe_read_leb128<uint32_t>(iter);
            fn.results.reserve(n_results);
            for (uint32_t j = 0; j < n_results; ++j) {
                if (!is_valtype(iter[j])) {
                    error<validation_error>("invalid result type");
                }
                fn.results.push_back(static_cast<valtype>(iter[j]));
            }
            iter += n_results;

            types.emplace_back(fn);
        }
    });

    skip_custom_section();

    uint32_t n_fn_imports = 0;

    // import section
    section(2, [&] {
        auto n_imports = safe_read_leb128<uint32_t>(iter);

        for (uint32_t i = 0; i < n_imports; i++) {
            if (iter.empty()) {
                error<malformed_error>("unexpected end of section or function");
            }

            auto module_len = safe_read_leb128<uint32_t>(iter);
            if (!is_valid_utf8(iter.get_with_at_least(module_len),
                               module_len)) {
                error<malformed_error>("malformed UTF-8 encoding");
            }
            auto module = std::string(
                reinterpret_cast<char *>(iter.get_with_at_least(module_len)),
                module_len);
            iter += module_len;

            auto field_len = safe_read_leb128<uint32_t>(iter);
            if (!is_valid_utf8(iter.get_with_at_least(field_len), field_len)) {
                error<malformed_error>("malformed UTF-8 encoding");
            }
            auto field = std::string(
                reinterpret_cast<char *>(iter.get_with_at_least(field_len)),
                field_len);
            iter += field_len;

            auto kind = *iter++;
            if (!is_imexdesc(kind)) {
                error<malformed_error>("malformed import kind");
            }
            auto desc = static_cast<ImExDesc>(kind);
            imports[module][field] = desc;

            auto specifier = std::make_optional(std::make_pair(module, field));

            if (desc == ImExDesc::func) {
                // func
                auto typeidx = safe_read_leb128<uint32_t>(iter);
                if (typeidx >= types.size()) {
                    error<validation_error>("unknown type");
                }
                functions.emplace_back(
                    FunctionShell(nullptr, types[typeidx], {}, {}, specifier));
                n_fn_imports++;
            } else if (desc == ImExDesc::table) {
                // table
                auto reftype = safe_read_leb128<uint32_t>(iter);
                if (!is_reftype(reftype)) {
                    error<malformed_error>("malformed reference type");
                }

                auto [initial, max] = get_table_limits(iter);
                tables.emplace_back(TableShell(
                    initial, max, static_cast<valtype>(reftype), specifier));
            } else if (desc == ImExDesc::mem) {
                // mem
                if (memory.exists) {
                    error<validation_error>("multiple memories");
                }

                auto [initial, max] = get_memory_limits(iter);
                this->memory = {initial, max, true, specifier};
            } else if (desc == ImExDesc::global) {
                // global
                auto maybe_valtype = safe_read_leb128<uint32_t>(iter);
                if (!is_valtype(maybe_valtype)) {
                    error<malformed_error>("invalid global type");
                }
                auto mutability = *iter++;
                if (!is_mut(mutability)) {
                    error<malformed_error>("malformed mutability");
                }

                globals.emplace_back(GlobalShell(
                    static_cast<valtype>(maybe_valtype),
                    static_cast<mut>(mutability), nullptr, specifier));
            }
        }
    });

    skip_custom_section();

    // function type section
    section(3, [&] {
        auto n_functions = safe_read_leb128<uint32_t>(iter);

        functions.reserve(n_functions);

        for (uint32_t i = 0; i < n_functions; ++i) {
            if (iter.empty()) {
                error<malformed_error>("unexpected end of section or function");
            }

            auto type_idx = safe_read_leb128<uint32_t>(iter);
            if (type_idx >= types.size()) {
                error<validation_error>("unknown type");
            }
            functions.emplace_back(
                FunctionShell(nullptr, types[type_idx], {}, {}, std::nullopt));
        }
    });

    skip_custom_section();

    // table section
    section(4, [&] {
        auto n_tables = safe_read_leb128<uint32_t>(iter);
        tables.reserve(n_tables);

        for (uint32_t i = 0; i < n_tables; ++i) {
            if (iter.empty()) {
                error<malformed_error>("unexpected end of section or function");
            }

            auto elem_type = *iter++;
            if (!is_reftype(elem_type)) {
                error<validation_error>("invalid table element type");
            }

            auto [initial, max] = get_table_limits(iter);
            tables.emplace_back(TableShell(
                initial, max, static_cast<valtype>(elem_type), std::nullopt));
        }
    });

    skip_custom_section();

    // memory section
    section(5, [&] {
        auto n_memories = safe_read_leb128<uint32_t>(iter);
        if (n_memories > 1) {
            error<validation_error>("multiple memories");
        } else if (n_memories == 1) {
            if (iter.empty()) {
                error<malformed_error>("unexpected end of section or function");
            }
            if (memory.exists) {
                error<validation_error>("multiple memories");
            }

            auto [initial, max] = get_memory_limits(iter);
            memory = {initial, max, true, std::nullopt};
        }
    });

    skip_custom_section();

    // global section
    section(6, [&] {
        auto n_globals = safe_read_leb128<uint32_t>(iter);

        globals.reserve(n_globals);

        for (uint32_t i = 0; i < n_globals; ++i) {
            if (iter.empty()) {
                error<malformed_error>("unexpected end of section or function");
            }

            auto maybe_type = *iter++;
            if (!is_valtype(maybe_type)) {
                error<malformed_error>("invalid global type");
            }
            auto type = static_cast<valtype>(maybe_type);

            auto maybe_mut = *iter++;
            if (!is_mut(maybe_mut)) {
                error<malformed_error>("malformed mutability");
            }
            auto global_mut = static_cast<mut>(maybe_mut);

            globals.emplace_back(
                GlobalShell(type, global_mut, iter.unsafe_ptr(), std::nullopt));

            validate_const(iter, type);
        }
    });

    skip_custom_section();

    // export section
    section(7, [&] {
        auto n_exports = safe_read_leb128<uint32_t>(iter);

        for (uint32_t i = 0; i < n_exports; ++i) {
            if (iter.empty()) {
                error<malformed_error>("unexpected end of section or function");
            }

            auto name_len = safe_read_leb128<uint32_t>(iter);
            auto name = std::string(
                reinterpret_cast<char *>(iter.get_with_at_least(name_len)),
                name_len);
            iter += name_len;

            auto desc = *iter++;
            if (!is_imexdesc(desc)) {
                error<validation_error>("invalid export description");
            }
            auto export_desc = static_cast<ImExDesc>(desc);

            auto idx = safe_read_leb128<uint32_t>(iter);

            if (exports.contains(name)) {
                error<validation_error>("duplicate export name");
            }

            if (export_desc == ImExDesc::func) {
                if (idx >= functions.size()) {
                    error<validation_error>("unknown function");
                }
                // implicit declaration
                functions[idx].is_declared = true;
            } else if (export_desc == ImExDesc::table) {
                if (idx >= tables.size()) {
                    error<validation_error>("unknown table");
                }
            } else if (export_desc == ImExDesc::mem) {
                if (idx != 0 || !memory.exists) {
                    error<validation_error>("unknown memory");
                }
            } else if (export_desc == ImExDesc::global) {
                if (idx >= globals.size()) {
                    error<validation_error>("unknown global");
                }
            }
            exports[name] = {export_desc, idx};
        }
    });

    skip_custom_section();

    section(
        8,
        [&] {
            start = safe_read_leb128<uint32_t>(iter);
            if (start >= functions.size()) {
                error<validation_error>("unknown function");
            }
        },
        [&] { start = std::numeric_limits<uint32_t>::max(); });

    skip_custom_section();

    // element section
    section(9, [&] {
        auto n_elements = safe_read_leb128<uint32_t>(iter);

        elements.reserve(n_elements);

        element_start = iter.unsafe_ptr();
        for (uint32_t i = 0; i < n_elements; i++) {
            if (iter.empty()) {
                error<malformed_error>("unexpected end of section or function");
            }

            auto flags = safe_read_leb128<uint32_t>(iter);
            if (flags & ~0b111) {
                error<validation_error>("invalid element flags");
            }

            if (flags & 1) {
                if (flags & 0b10) {
                    if (flags & 0b100) {
                        // flags = 7
                        // characteristics: declarative, elem type + exprs
                        auto maybe_reftype = *iter++;
                        if (!is_reftype(maybe_reftype)) {
                            error<malformed_error>("malformed reference type");
                        }
                        auto reftype = static_cast<valtype>(maybe_reftype);
                        auto n_elements = safe_read_leb128<uint32_t>(iter);
                        for (uint32_t j = 0; j < n_elements; j++) {
                            // validate_const sets is_declared
                            validate_const(iter, reftype);
                        }
                        elements.emplace_back(ElementShell(reftype));
                    } else {
                        // flags = 3
                        // characteristics: declarative, elem kind + indices
                        auto elemkind = *iter++;
                        if (elemkind != 0) {
                            error<validation_error>("invalid elemkind");
                        }
                        auto n_elements = safe_read_leb128<uint32_t>(iter);
                        for (uint32_t j = 0; j < n_elements; j++) {
                            auto elem_idx = safe_read_leb128<uint32_t>(iter);
                            if (elem_idx >= functions.size()) {
                                error<validation_error>("unknown function");
                            }
                            functions[elem_idx].is_declared = true;
                        }
                        elements.emplace_back(ElementShell(valtype::funcref));
                    }
                } else {
                    if (flags & 0b100) {
                        // flags = 5
                        // characteristics: passive, elem type + exprs
                        auto maybe_reftype = *iter++;
                        if (!is_reftype(maybe_reftype)) {
                            error<malformed_error>("malformed reference type");
                        }
                        auto reftype = static_cast<valtype>(maybe_reftype);
                        auto n_elements = safe_read_leb128<uint32_t>(iter);
                        for (uint32_t j = 0; j < n_elements; j++) {
                            validate_const(iter, reftype);
                        }
                        elements.emplace_back(ElementShell(reftype));
                    } else {
                        // flags = 1
                        // characteristics: passive, elem kind + indices
                        auto elemkind = *iter++;
                        if (elemkind != 0) {
                            error<validation_error>("invalid elemkind");
                        }
                        auto n_elements = safe_read_leb128<uint32_t>(iter);
                        for (uint32_t j = 0; j < n_elements; j++) {
                            auto elem_idx = safe_read_leb128<uint32_t>(iter);
                            if (elem_idx >= functions.size()) {
                                error<validation_error>("unknown function");
                            }
                            // implicit declaration
                            functions[elem_idx].is_declared = true;
                        }
                        elements.emplace_back(ElementShell(valtype::funcref));
                    }
                }
            } else {
                auto table_idx =
                    flags & 0b10 ? safe_read_leb128<uint32_t>(iter) : 0;
                if (table_idx >= tables.size()) {
                    error<validation_error>("unknown table");
                }

                auto reftype = valtype::null;

                /* auto offset = */ validate_const(iter, valtype::i32);
                auto reftype_or_elemkind = flags & 0b10 ? *iter++ : 256;
                auto n_elements = safe_read_leb128<uint32_t>(iter);

                if (flags & 0b100) {
                    // flags = 4 or 6
                    // characteristics: active, elem type + exprs
                    if (reftype_or_elemkind == 256)
                        reftype_or_elemkind =
                            static_cast<uint16_t>(valtype::funcref);
                    if (!is_reftype(reftype_or_elemkind)) {
                        error<malformed_error>("malformed reference type");
                    }
                    reftype = static_cast<valtype>(reftype_or_elemkind);
                    if (tables[table_idx].type != reftype) {
                        error<validation_error>("type mismatch");
                    }
                    for (uint32_t j = 0; j < n_elements; j++) {
                        validate_const(iter, reftype);
                    }
                } else {
                    if (reftype_or_elemkind == 256)
                        reftype_or_elemkind = 0;
                    if (reftype_or_elemkind != 0) {
                        error<validation_error>("invalid elemkind");
                    }
                    reftype = valtype::funcref;
                    if (tables[table_idx].type != reftype) {
                        error<validation_error>("type mismatch");
                    }
                    // flags = 0 or 2
                    // characteristics: active, elem kind + indices
                    for (uint32_t j = 0; j < n_elements; j++) {
                        auto elem_idx = safe_read_leb128<uint32_t>(iter);
                        if (elem_idx >= functions.size()) {
                            error<validation_error>("unknown function");
                        }
                        // implicit declaration
                        functions[elem_idx].is_declared = true;
                    }
                }
                elements.emplace_back(ElementShell(reftype));
            }
        }
    });

    skip_custom_section();

    // data count section
    n_data = std::numeric_limits<uint32_t>::max();
    auto has_data_count = false;
    section(12, [&] {
        n_data = safe_read_leb128<uint32_t>(iter);
        has_data_count = true;
    });

    skip_custom_section();

    // code section
    section(
        10,
        [&](uint32_t section_length) {
            auto n_functions = safe_read_leb128<uint32_t>(iter);

            if (n_functions + n_fn_imports != functions.size()) {
                error<malformed_error>(
                    "function and code section have inconsistent lengths");
            }

            auto ludes = n_functions *
                         (Target::max_prelude_size + Target::max_postlude_size);

            auto other = section_length *
                         (Target::max_call_size + Target::max_temp1_size +
                          Target::max_temp2_size);

            executable =
                Pager::allocate(ludes + other, AllocationKind::Executable);

            auto code = executable.get();
            Pager::write(executable, [&] {
                for (FunctionShell &fn : functions) {
                    if (fn.import) {
                        // skip imported functions
                        continue;
                    }

                    fn.locals = fn.type.params;

                    auto function_length = safe_read_leb128<uint32_t>(iter);

                    auto start = safe_byte_iterator(
                        iter.get_with_at_least(function_length),
                        function_length);

                    auto n_local_decls = safe_read_leb128<uint32_t>(iter);
                    while (n_local_decls--) {
                        auto n_locals = safe_read_leb128<uint32_t>(iter);
                        auto type = *iter++;
                        if (!is_valtype(type)) {
                            error<validation_error>("invalid local type");
                        }
                        while (n_locals--) {
                            fn.locals.push_back(static_cast<valtype>(type));
                            if (fn.locals.size() > MAX_LOCALS) {
                                error<malformed_error>("too many locals");
                            }
                        }
                    }
                    fn.local_bytes.reserve(fn.locals.size());
                    uint32_t psum = 0;
                    for (auto it = fn.locals.rbegin(); it != fn.locals.rend();
                         ++it) {
                        psum += valtype_size(*it);
                        fn.local_bytes.push_back(psum);
                    }
                    std::reverse(fn.local_bytes.begin(), fn.local_bytes.end());

                    auto body_length = function_length - (iter - start);
                    fn.start =
                        reinterpret_cast<runtime::TemplessSignature *>(code);
                    if (!iter.has_n_left(body_length)) {
                        error<malformed_error>("length out of bounds");
                    }

#ifdef WASM_DEBUG
                    std::cerr << "validating function "
                              << &fn - functions.data() << " at "
                              << iter - bytes.data() << std::endl;
#endif
                    auto fn_iter = iter;
                    code =
                        validate_and_compile<Pager, Target>(fn_iter, code, fn);
                    if (fn_iter[-1] != static_cast<uint8_t>(Instruction::end)) {
                        error<malformed_error>("END opcode expected");
                    }
                    if (fn_iter - start != function_length) {
                        error<malformed_error>("section size mismatch");
                    }
                    iter = fn_iter;
                }

                for (auto [call, func_idx] : pending_calls) {
                    Target::put_temp1(call, reinterpret_cast<uint64_t>(
                                                functions[func_idx].start));
                }

                return code - executable.get();
            });
        },
        [&] {
            if (functions.size() != n_fn_imports) {
                error<malformed_error>(
                    "function and code section have inconsistent lengths");
            }
        });

    skip_custom_section();

    // data section
    section(
        11,
        [&] {
            auto section_n_data = safe_read_leb128<uint32_t>(iter);
            if (has_data_count && n_data != section_n_data) {
                error<malformed_error>("data count and data section have "
                                       "inconsistent lengths");
            }

            for (uint32_t i = 0; i < section_n_data; i++) {
                if (iter.empty()) {
                    error<malformed_error>(
                        "unexpected end of section or function");
                }

                auto segment_flag = safe_read_leb128<uint32_t>(iter);
                if (segment_flag & ~0b11) {
                    error<validation_error>("invalid data segment flag");
                }

                auto memidx =
                    segment_flag & 0b10 ? safe_read_leb128<uint32_t>(iter) : 0;

                if (memidx != 0) {
                    error<validation_error>("unknown memory");
                }

                if (segment_flag & 1) {
                    // passive segment

                    auto data_length = safe_read_leb128<uint32_t>(iter);
                    if (!iter.has_n_left(data_length)) {
                        error<malformed_error>(
                            "unexpected end of section or function");
                    }

                    auto segment = std::make_unique<uint8_t[]>(data_length);
                    std::memcpy(segment.get(),
                                iter.get_with_at_least(data_length),
                                data_length);
                    iter += data_length;

                    data_segments.emplace_back(runtime::Segment(
                        memidx, data_length, std::move(segment), nullptr));
                } else {
                    // active segment
                    if (!memory.exists) {
                        error<validation_error>("unknown memory 0");
                    }

                    auto initializer = iter.unsafe_ptr();
                    validate_const(iter, valtype::i32);
                    auto data_length = safe_read_leb128<uint32_t>(iter);
                    if (!iter.has_n_left(data_length)) {
                        error<malformed_error>(
                            "unexpected end of section or function");
                    }

                    auto segment = std::make_unique<uint8_t[]>(data_length);
                    std::memcpy(segment.get(),
                                iter.get_with_at_least(data_length),
                                data_length);
                    iter += data_length;

                    // todo: this has to be instantiated
                    data_segments.emplace_back(runtime::Segment(
                        memidx, data_length, std::move(segment), initializer));
                }
            }
        },
        [&] {
            if (has_data_count && n_data != 0) {
                error<malformed_error>("data count and data section have "
                                       "inconsistent lengths");
            }
        });

    skip_custom_section();

    if (!iter.empty()) {
        error<malformed_error>("unexpected content after last section");
    }
}

template <typename T> auto WasmStack::find_diverging(const T &expected) const {
    auto ebegin = expected.rbegin();
    auto abegin = rbegin();

    for (; ebegin != expected.rend(); ++abegin, ++ebegin)
        if (*abegin != *ebegin && *abegin != valtype::any)
            break;

    return abegin;
}

template <typename T> bool WasmStack::check(const T &expected) const {
    auto diverge = find_diverging(expected);
    if (static_cast<size_t>(std::distance(rbegin(), diverge)) ==
        expected.size())
        return true;
    return polymorphized && *diverge == valtype::null;
}

template <typename T> bool WasmStack::operator==(const T &rhs) const {
    auto diverge = find_diverging(rhs);
    if (*diverge != valtype::null)
        return false;
    if (static_cast<size_t>(std::distance(rbegin(), diverge)) == rhs.size())
        return true;
    return polymorphized;
}

template <typename T> void WasmStack::push(const T &values) {
    std::copy(values.begin(), values.end(), buffer);
    buffer += values.size();
    stack_size +=
        std::reduce(values.begin(), values.end(), 0,
                    [](auto a, auto b) { return a + valtype_size(b); });
}
template <typename T> void WasmStack::pop(const T &expected) {
    ensure(check(expected), "type mismatch");

    auto diverge = find_diverging(expected);
    buffer -= std::distance(rbegin(), diverge);
    stack_size -=
        std::reduce(expected.begin(), expected.end(), 0,
                    [](auto a, auto b) { return a + valtype_size(b); });
}

template <size_t pc, size_t rc>
void WasmStack::apply(std::array<valtype, pc> params,
                      std::array<valtype, rc> results) {
    pop(params);
    push(results);
}

#define LOAD(type, stacktype, name)                                            \
    {                                                                          \
        auto a = safe_read_leb128<uint32_t>(iter);                             \
        ensure(mod.memory.exists, "unknown memory");                           \
        if (a >= 32) {                                                         \
            error<malformed_error>("malformed memop flags");                   \
        }                                                                      \
        auto align = 1ull << a;                                                \
        ensure(align <= sizeof(type),                                          \
               "alignment must not be larger than natural");                   \
        auto offset = safe_read_leb128<uint32_t>(iter);                        \
        stack.apply(std::array{valtype::i32}, std::array{stacktype});          \
        Target::put_temp1(code, offset);                                       \
        Target::put_call(code, runtime::name);                                 \
        nextop();                                                              \
    }

#define STORE(type, stacktype, name)                                           \
    {                                                                          \
        auto a = safe_read_leb128<uint32_t>(iter);                             \
        if ((1 << 6) & a) {                                                    \
            a -= 1 << 6;                                                       \
            /* todo: test multi memory proposal */                             \
            a = safe_read_leb128<uint32_t>(iter);                              \
        } else {                                                               \
            ensure(mod.memory.exists, "unknown memory");                       \
        }                                                                      \
        auto align = 1ull << a;                                                \
        ensure(align <= sizeof(type),                                          \
               "alignment must not be larger than natural");                   \
        auto offset = safe_read_leb128<uint32_t>(iter);                        \
        stack.apply(std::array{valtype::i32, stacktype},                       \
                    std::array<valtype, 0>());                                 \
        Target::put_temp1(code, offset);                                       \
        Target::put_call(code, runtime::name);                                 \
        nextop();                                                              \
    }

#define HANDLER(name)                                                          \
    template <typename Target>                                                 \
    uint8_t *validate_##name(Module &mod, safe_byte_iterator &iter,            \
                             FunctionShell &fn, WasmStack &stack,              \
                             std::vector<ControlFlow> &control_stack,          \
                             uint8_t *code)

#define V(name, _, byte) HANDLER(name);
FOREACH_INSTRUCTION(V)
#undef V

#ifdef WASM_DEBUG
#define nextop()                                                               \
    do {                                                                       \
        auto byte = *iter++;                                                   \
        std::cerr << "reading instruction " << instructions[byte].c_str()      \
                  << std::endl;                                                \
        std::cerr << "control stack size: " << control_stack.size()            \
                  << std::endl;                                                \
        std::cerr << "control stack: ";                                        \
        for (auto &target : control_stack) {                                   \
            std::cerr << target.expected.size() << " ";                        \
        }                                                                      \
        std::cerr << std::endl;                                                \
        std::cerr << "stack size: " << stack.size() << std::endl;              \
        std::cerr << "stack: ";                                                \
        for (auto &ty : stack) {                                               \
            std::cerr << valtype_names[static_cast<uint8_t>(ty)].c_str()       \
                      << " ";                                                  \
        }                                                                      \
        std::cerr << std::endl;                                                \
        std::cerr << std::endl;                                                \
        [[clang::musttail]] return funcs<Target>[byte](mod, iter, fn, stack,   \
                                                       control_stack, code);   \
    } while (0)
#else
#define nextop()                                                               \
    do {                                                                       \
        auto byte = *iter++;                                                   \
        [[clang::musttail]] return funcs<Target>[byte](mod, iter, fn, stack,   \
                                                       control_stack, code);   \
    } while (0)
#endif

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-parameter"

HANDLER(missing) { error<malformed_error>("invalid instruction"); }

template <typename Target>
consteval std::array<CompilationHandler *, 256> make_funcs() {
    std::array<CompilationHandler *, 256> funcs;
    funcs.fill(validate_missing<Target>);

#define V(name, _, byte) funcs[byte] = &validate_##name<Target>;
    FOREACH_INSTRUCTION(V)
#undef V

    return funcs;
}

template <typename Target> static auto funcs = make_funcs<Target>();

template <typename T> void put(uint8_t *&code, const T &value) {
    std::memcpy(code, &value, sizeof(T));
    code += sizeof(T);
}

HANDLER(unreachable) {
    stack.polymorphize();

    Target::put_call(code, runtime::unreachable);
    nextop();
}
HANDLER(nop) { nextop(); }
HANDLER(block) {
    auto &signature = read_blocktype(mod.types, iter);

    stack.enter_flow(signature.params);
    control_stack.emplace_back(
        ControlFlow(signature.results, {}, {}, signature, stack.polymorphism(),
                    stack.sp() - signature.params.bytesize(), Block()));
    stack.unpolymorphize();
    nextop();
}
HANDLER(loop) {
    auto &signature = read_blocktype(mod.types, iter);

    stack.enter_flow(signature.params);
    control_stack.emplace_back(
        ControlFlow(signature.params, {}, {}, signature, stack.polymorphism(),
                    stack.sp() - signature.params.bytesize(), Loop(code)));
    stack.unpolymorphize();
    nextop();
}
HANDLER(if_) {
    auto &signature = read_blocktype(mod.types, iter);

    auto if_ = If(Target::put_if(code));

    stack.pop(valtype::i32);
    stack.enter_flow(signature.params);
    control_stack.emplace_back(
        ControlFlow(signature.results, {}, {}, signature, stack.polymorphism(),
                    stack.sp() - signature.params.bytesize(), if_));
    stack.unpolymorphize();
    nextop();
}
HANDLER(else_) {
    auto &[expected, pending_br, pending_br_tables, sig, _, stack_offset,
           construct] = control_stack.back();
    ensure(std::holds_alternative<If>(construct), "else must close an if");
    ensure(stack == sig.results, "type mismatch");

    stack.pop(sig.results);
    stack.push(sig.params);

    // necessary in case of polymorphic stack
    stack.set_sp(stack_offset + sig.params.bytesize());

    auto [else_jump] = std::get<If>(construct);
    // jump to end of if/else block after if block
    pending_br.push_back(Target::put_br(code, 0, 0));
    else_jump.set(code);

    control_stack.back().construct = IfElse();
    stack.unpolymorphize();
    nextop();
}
HANDLER(end) {
    auto &[_, pending_br, pending_br_tables, sig, polymorphism, sp, construct] =
        control_stack.back();

    ensure(stack == sig.results, "type mismatch stack vs. results");

    if (std::holds_alternative<If>(construct)) {
        ensure(sig.params == sig.results, "type mismatch params vs. results");
        std::get<If>(construct).else_jump.set(code);
    }

    // everything except loop jumps to end
    if (!std::holds_alternative<Loop>(construct)) {
        for (auto target : pending_br) {
            target.set(code);
        }
        for (auto [table, target] : pending_br_tables) {
            auto diff = code - table;
            auto idiff = static_cast<int32_t>(diff);
            ensure(idiff == diff, "branch target out of range");
            std::memcpy(target, &idiff, sizeof(idiff));
        }
    }

    if (std::holds_alternative<Function>(construct)) {
        // move results past locals
        Target::put_temp1(code, -fn.locals.bytesize());
        auto byte_results = sig.results.bytesize();
        if (byte_results == 0) {
            Target::put_call(code, runtime::move_0_results);
        } else if (byte_results == 8) {
            Target::put_call(code, runtime::move_8_results);
        } else {
            Target::put_temp2(code, -byte_results);
            Target::put_call(code, runtime::move_n_results);
        }

        Target::put_postlude(code);
        return code;
    }

    stack.pop(sig.results);
    stack.pop(valtype::null);

    stack.set_polymorphism(polymorphism);

    // necessary in case of polymorphic stack
    stack.set_sp(sp + sig.params.bytesize());

    stack.push(sig.results);

    control_stack.pop_back();
    nextop();
}
HANDLER(br) {
    auto depth = safe_read_leb128<uint32_t>(iter);
    stack.check_br(control_stack, depth);
    auto &flow = control_stack[control_stack.size() - depth - 1];

    auto imm = Target::put_br(code, flow.expected.bytesize(),
                              flow.stack_offset - stack.sp());

    if (std::holds_alternative<Loop>(flow.construct)) {
        imm.set(std::get<Loop>(flow.construct).start);
    } else {
        flow.pending_br.push_back(imm);
    }

    stack.polymorphize();
    nextop();
}
HANDLER(br_if) {
    stack.pop(valtype::i32);

    auto depth = safe_read_leb128<uint32_t>(iter);
    stack.check_br(control_stack, depth);
    auto &flow = control_stack[control_stack.size() - depth - 1];

    auto imm = Target::put_br_if(code, flow.expected.bytesize(),
                                 flow.stack_offset - stack.sp());

    if (std::holds_alternative<Loop>(flow.construct)) {
        imm.set(std::get<Loop>(flow.construct).start);
    } else {
        flow.pending_br.push_back(imm);
    }

    nextop();
}
HANDLER(br_table) {
    stack.pop(valtype::i32);
    // todo: maybe a different function for low n could be good?
    auto n_targets = safe_read_leb128<uint32_t>(iter);

    auto t1_addr = code;
    Target::placehold(code, Target::put_temp1);
    auto t2_addr = code;
    Target::placehold(code, Target::put_temp2);
    auto call_addr = code;
    Target::placehold(code, Target::put_call);
    auto table_addr = code;

    Target::put_temp1(t1_addr, reinterpret_cast<uint64_t>(table_addr));

    auto targets = (uint32_t *)alloca(sizeof(uint32_t) * (n_targets + 1));
    for (uint32_t i = 0; i <= n_targets; ++i) {
        auto target = safe_read_leb128<uint32_t>(iter);
        ensure(target < control_stack.size(), "unknown label");
        targets[i] = target;
    }
    auto base = control_stack.size() - 1;
    auto &default_target = control_stack[base - targets[n_targets]].expected;

    auto info = runtime::BrInfo(n_targets, default_target.bytesize());
    if (info.arity == 0) {
        Target::put_call(call_addr, runtime::br_table_0);
    } else if (info.arity == 8) {
        Target::put_call(call_addr, runtime::br_table_8);
    } else {
        Target::put_call(call_addr, runtime::br_table_n);
    }
    Target::put_temp2(t2_addr, std::bit_cast<uint64_t>(info));

    for (uint32_t i = 0; i <= n_targets; ++i) {
        auto &target = control_stack[base - targets[i]].expected;
        if (stack.can_be_anything()) {
            ensure(stack.check(target), "type mismatch");
            ensure(default_target.size() == target.size(), "type mismatch");
        } else {
            stack.check_br(control_stack, targets[i]);

            auto &flow = control_stack[control_stack.size() - targets[i] - 1];
            auto offset = flow.stack_offset - stack.sp();
            if (std::holds_alternative<Loop>(flow.construct)) {
                put(code, runtime::BrTableTarget(
                              std::get<Loop>(flow.construct).start - table_addr,
                              offset));
            } else {
                flow.pending_br_tables.push_back(
                    PendingBrTable(table_addr, code));
                code += sizeof(uint32_t);
                put(code, static_cast<uint32_t>(offset));
            }

            ensure(default_target == target, "type mismatch");
        }
    }
    stack.polymorphize();
    nextop();
}
HANDLER(return_) {
    stack.check_br(control_stack, control_stack.size() - 1);

    auto &flow = control_stack.front();
    auto imm = Target::put_br(code, flow.expected.bytesize(),
                              flow.stack_offset - stack.sp());

    if (std::holds_alternative<Loop>(flow.construct)) {
        imm.set(std::get<Loop>(flow.construct).start);
    } else {
        flow.pending_br.push_back(imm);
    }

    stack.polymorphize();
    nextop();
}
HANDLER(call) {
    auto fn_idx = safe_read_leb128<uint32_t>(iter);
    ensure(fn_idx < mod.functions.size(), "unknown function");

    auto &func = mod.functions[fn_idx];
    stack.apply(func.type);

    if (func.import) {
        Target::put_temp1(code, 1 + fn_idx);
        Target::put_temp2(code, func.type.results.bytesize() -
                                    func.type.params.bytesize());
        Target::put_call(code, runtime::call_extern);
    } else {
        mod.pending_calls.push_back({code, fn_idx});
        Target::placehold(code, Target::put_temp1);
        Target::put_temp2(code, func.type.results.bytesize() -
                                    func.type.params.bytesize());
        Target::put_call(code, runtime::call);
    }
    nextop();
}
HANDLER(call_indirect) {
    stack.pop(valtype::i32);

    auto type_idx = safe_read_leb128<uint32_t>(iter);
    ensure(type_idx < mod.types.size(), "unknown type");

    auto table_idx = safe_read_leb128<uint32_t>(iter);
    ensure(table_idx < mod.tables.size(), "unknown table");
    ensure(mod.tables[table_idx].type == valtype::funcref, "type mismatch");

    auto &type = mod.types[type_idx];
    stack.apply(type);

    auto info = runtime::CallIndirectInfo(1 + mod.functions.size() + table_idx,
                                          runtime::FunctionType(type));
    auto [temp1, temp2] = std::bit_cast<std::array<uint64_t, 2>>(info);

    Target::put_temp1(code, temp1);
    Target::put_temp2(code, temp2);
    Target::put_call(code, runtime::call_indirect);
    nextop();
}
HANDLER(drop) {
    stack.pop(stack.back());

    Target::put_call(code, runtime::drop);
    nextop();
}
HANDLER(select) {
    // first pop the condition
    stack.pop(valtype::i32);

    auto ty = stack.back();
    ensure(ty == valtype::any || is_numtype(ty), "type mismatch");

    // then apply the dynamic type
    stack.apply(std::array{ty, ty}, std::array{ty});

    Target::put_call(code, runtime::select);
    nextop();
}
HANDLER(select_t) {
    auto n_results = safe_read_leb128<uint32_t>(iter);
    ensure(n_results == 1, "invalid result arity");
    auto maybe_valtype = *iter++;
    ensure(is_valtype(maybe_valtype), "invalid result type");

    // first pop the condition
    stack.pop(valtype::i32);
    auto ty = stack.back();
    // then apply the dynamic type
    stack.apply(std::array{ty, ty}, std::array{ty});

    Target::put_call(code, runtime::select_t);
    nextop();
}
HANDLER(localget) {
    auto local_idx = safe_read_leb128<uint32_t>(iter);
    ensure(local_idx < fn.locals.size(), "unknown local");

    Target::put_temp1(code, -(stack.sp() + fn.local_bytes[local_idx]));
    Target::put_call(code, runtime::localget);

    auto local_ty = fn.locals[local_idx];
    stack.apply(std::array<valtype, 0>(), std::array{local_ty});
    nextop();
}
HANDLER(localset) {
    auto local_idx = safe_read_leb128<uint32_t>(iter);
    ensure(local_idx < fn.locals.size(), "unknown local");

    Target::put_temp1(code, -(stack.sp() + fn.local_bytes[local_idx]));
    Target::put_call(code, runtime::localset);

    auto local_ty = fn.locals[local_idx];
    stack.apply(std::array{local_ty}, std::array<valtype, 0>());
    nextop();
}
HANDLER(localtee) {
    auto local_idx = safe_read_leb128<uint32_t>(iter);
    ensure(local_idx < fn.locals.size(), "unknown local");

    Target::put_temp1(code, -(stack.sp() + fn.local_bytes[local_idx]));
    Target::put_call(code, runtime::localtee);

    auto local_ty = fn.locals[local_idx];
    stack.apply(std::array{local_ty}, std::array{local_ty});
    nextop();
}
HANDLER(tableget) {
    auto table_idx = safe_read_leb128<uint32_t>(iter);
    ensure(table_idx < mod.tables.size(), "unknown table");
    auto table_ty = mod.tables[table_idx].type;
    stack.apply(std::array{valtype::i32}, std::array{table_ty});

    Target::put_temp1(code, 1 + mod.functions.size() + table_idx);
    Target::put_call(code, runtime::tableget);
    nextop();
}
HANDLER(tableset) {
    auto table_idx = safe_read_leb128<uint32_t>(iter);
    ensure(table_idx < mod.tables.size(), "unknown table");
    auto table_ty = mod.tables[table_idx].type;
    stack.apply(std::array{valtype::i32, table_ty}, std::array<valtype, 0>());

    Target::put_temp1(code, 1 + mod.functions.size() + table_idx);
    Target::put_call(code, runtime::tableset);
    nextop();
}
HANDLER(globalget) {
    auto global_idx = safe_read_leb128<uint32_t>(iter);
    ensure(global_idx < mod.globals.size(), "unknown global");
    auto global_ty = mod.globals[global_idx].type;
    stack.apply(std::array<valtype, 0>(), std::array{global_ty});

    Target::put_temp1(code, 1 + mod.functions.size() + mod.tables.size() +
                                global_idx);
    Target::put_call(code, runtime::globalget);
    nextop();
}
HANDLER(globalset) {
    auto global_idx = safe_read_leb128<uint32_t>(iter);
    ensure(global_idx < mod.globals.size(), "unknown global");
    ensure(mod.globals[global_idx].mutability == mut::var,
           "global is immutable");
    auto global_ty = mod.globals[global_idx].type;
    stack.apply(std::array{global_ty}, std::array<valtype, 0>());

    Target::put_temp1(code, 1 + mod.functions.size() + mod.tables.size() +
                                global_idx);
    Target::put_call(code, runtime::globalset);
    nextop();
}
HANDLER(memorysize) {
    if (*iter++ != 0)
        error<malformed_error>("zero byte expected");
    ensure(mod.memory.exists, "unknown memory");
    stack.apply(std::array<valtype, 0>(), std::array{valtype::i32});

    Target::put_call(code, runtime::memorysize);
    nextop();
}
HANDLER(memorygrow) {
    if (*iter++ != 0)
        error<malformed_error>("zero byte expected");
    ensure(mod.memory.exists, "unknown memory");
    stack.apply(std::array{valtype::i32}, std::array{valtype::i32});

    Target::put_call(code, runtime::memorygrow);
    nextop();
}
HANDLER(i32const) {
    runtime::WasmValue v = safe_read_sleb128<uint32_t>(iter);
    stack.apply(std::array<valtype, 0>(), std::array{valtype::i32});

    Target::put_temp1(code, v.u64);
    Target::put_call(code, runtime::ifXXconst);
    nextop();
}
HANDLER(i64const) {
    runtime::WasmValue v = safe_read_sleb128<uint64_t>(iter);
    stack.apply(std::array<valtype, 0>(), std::array{valtype::i64});

    Target::put_temp1(code, v.u64);
    Target::put_call(code, runtime::ifXXconst);
    nextop();
}
HANDLER(f32const) {
    runtime::WasmValue v;
    std::memcpy(&v.f32, iter.get_with_at_least(sizeof(float)), sizeof(float));
    iter += sizeof(float);
    stack.apply(std::array<valtype, 0>(), std::array{valtype::f32});

    Target::put_temp1(code, v.u64);
    Target::put_call(code, runtime::ifXXconst);
    nextop();
}
HANDLER(f64const) {
    runtime::WasmValue v;
    std::memcpy(&v.f64, iter.get_with_at_least(sizeof(double)), sizeof(double));
    iter += sizeof(double);
    stack.apply(std::array<valtype, 0>(), std::array{valtype::f64});

    Target::put_temp1(code, v.u64);
    Target::put_call(code, runtime::ifXXconst);
    nextop();
}
// clang-format off
HANDLER(i32load) {     LOAD(uint32_t,  valtype::i32, i32load); }
HANDLER(i64load) {     LOAD(uint64_t,  valtype::i64, i64load); }
HANDLER(f32load) {     LOAD(float,     valtype::f32, f32load); }
HANDLER(f64load) {     LOAD(double,    valtype::f64, f64load); }
HANDLER(i32load8_s) {  LOAD(int8_t,    valtype::i32, i32load8_s); }
HANDLER(i32load8_u) {  LOAD(uint8_t,   valtype::i32, i32load8_u); }
HANDLER(i32load16_s) { LOAD(int16_t,   valtype::i32, i32load16_s); }
HANDLER(i32load16_u) { LOAD(uint16_t,  valtype::i32, i32load16_u); }
HANDLER(i64load8_s) {  LOAD(int8_t,    valtype::i64, i64load8_s); }
HANDLER(i64load8_u) {  LOAD(uint8_t,   valtype::i64, i64load8_u); }
HANDLER(i64load16_s) { LOAD(int16_t,   valtype::i64, i64load16_s); }
HANDLER(i64load16_u) { LOAD(uint16_t,  valtype::i64, i64load16_u); }
HANDLER(i64load32_s) { LOAD(int32_t,   valtype::i64, i64load32_s); }
HANDLER(i64load32_u) { LOAD(uint32_t,  valtype::i64, i64load32_u); }
HANDLER(i32store) {    STORE(uint32_t, valtype::i32, i32store); }
HANDLER(i64store) {    STORE(uint64_t, valtype::i64, i64store); }
HANDLER(f32store) {    STORE(float,    valtype::f32, f32store); }
HANDLER(f64store) {    STORE(double,   valtype::f64, f64store); }
HANDLER(i32store8) {   STORE(uint8_t,  valtype::i32, i32store8); }
HANDLER(i32store16) {  STORE(uint16_t, valtype::i32, i32store16); }
HANDLER(i64store8) {   STORE(uint8_t,  valtype::i64, i64store8); }
HANDLER(i64store16) {  STORE(uint16_t, valtype::i64, i64store16); }
HANDLER(i64store32) {  STORE(uint32_t, valtype::i64, i64store32); }
HANDLER(i32eqz) {      stack.apply(std::array{valtype::i32              }, std::array{valtype::i32}); Target::put_call(code, runtime::i32eqz); nextop(); }
HANDLER(i64eqz) {      stack.apply(std::array{valtype::i64              }, std::array{valtype::i32}); Target::put_call(code, runtime::i64eqz); nextop(); }
HANDLER(i32eq) {       stack.apply(std::array{valtype::i32, valtype::i32}, std::array{valtype::i32}); Target::put_call(code, runtime::i32eq); nextop(); }
HANDLER(i64eq) {       stack.apply(std::array{valtype::i64, valtype::i64}, std::array{valtype::i32}); Target::put_call(code, runtime::i64eq); nextop(); }
HANDLER(i32ne) {       stack.apply(std::array{valtype::i32, valtype::i32}, std::array{valtype::i32}); Target::put_call(code, runtime::i32ne); nextop(); }
HANDLER(i64ne) {       stack.apply(std::array{valtype::i64, valtype::i64}, std::array{valtype::i32}); Target::put_call(code, runtime::i64ne); nextop(); }
HANDLER(i32lt_s) {     stack.apply(std::array{valtype::i32, valtype::i32}, std::array{valtype::i32}); Target::put_call(code, runtime::i32lt_s); nextop(); }
HANDLER(i64lt_s) {     stack.apply(std::array{valtype::i64, valtype::i64}, std::array{valtype::i32}); Target::put_call(code, runtime::i64lt_s); nextop(); }
HANDLER(i32lt_u) {     stack.apply(std::array{valtype::i32, valtype::i32}, std::array{valtype::i32}); Target::put_call(code, runtime::i32lt_u); nextop(); }
HANDLER(i64lt_u) {     stack.apply(std::array{valtype::i64, valtype::i64}, std::array{valtype::i32}); Target::put_call(code, runtime::i64lt_u); nextop(); }
HANDLER(i32gt_s) {     stack.apply(std::array{valtype::i32, valtype::i32}, std::array{valtype::i32}); Target::put_call(code, runtime::i32gt_s); nextop(); }
HANDLER(i64gt_s) {     stack.apply(std::array{valtype::i64, valtype::i64}, std::array{valtype::i32}); Target::put_call(code, runtime::i64gt_s); nextop(); }
HANDLER(i32gt_u) {     stack.apply(std::array{valtype::i32, valtype::i32}, std::array{valtype::i32}); Target::put_call(code, runtime::i32gt_u); nextop(); }
HANDLER(i64gt_u) {     stack.apply(std::array{valtype::i64, valtype::i64}, std::array{valtype::i32}); Target::put_call(code, runtime::i64gt_u); nextop(); }
HANDLER(i32le_s) {     stack.apply(std::array{valtype::i32, valtype::i32}, std::array{valtype::i32}); Target::put_call(code, runtime::i32le_s); nextop(); }
HANDLER(i64le_s) {     stack.apply(std::array{valtype::i64, valtype::i64}, std::array{valtype::i32}); Target::put_call(code, runtime::i64le_s); nextop(); }
HANDLER(i32le_u) {     stack.apply(std::array{valtype::i32, valtype::i32}, std::array{valtype::i32}); Target::put_call(code, runtime::i32le_u); nextop(); }
HANDLER(i64le_u) {     stack.apply(std::array{valtype::i64, valtype::i64}, std::array{valtype::i32}); Target::put_call(code, runtime::i64le_u); nextop(); }
HANDLER(i32ge_s) {     stack.apply(std::array{valtype::i32, valtype::i32}, std::array{valtype::i32}); Target::put_call(code, runtime::i32ge_s); nextop(); }
HANDLER(i64ge_s) {     stack.apply(std::array{valtype::i64, valtype::i64}, std::array{valtype::i32}); Target::put_call(code, runtime::i64ge_s); nextop(); }
HANDLER(i32ge_u) {     stack.apply(std::array{valtype::i32, valtype::i32}, std::array{valtype::i32}); Target::put_call(code, runtime::i32ge_u); nextop(); }
HANDLER(i64ge_u) {     stack.apply(std::array{valtype::i64, valtype::i64}, std::array{valtype::i32}); Target::put_call(code, runtime::i64ge_u); nextop(); }
HANDLER(f32eq) {       stack.apply(std::array{valtype::f32, valtype::f32}, std::array{valtype::i32}); Target::put_call(code, runtime::f32eq); nextop(); }
HANDLER(f64eq) {       stack.apply(std::array{valtype::f64, valtype::f64}, std::array{valtype::i32}); Target::put_call(code, runtime::f64eq); nextop(); }
HANDLER(f32ne) {       stack.apply(std::array{valtype::f32, valtype::f32}, std::array{valtype::i32}); Target::put_call(code, runtime::f32ne); nextop(); }
HANDLER(f64ne) {       stack.apply(std::array{valtype::f64, valtype::f64}, std::array{valtype::i32}); Target::put_call(code, runtime::f64ne); nextop(); }
HANDLER(f32lt) {       stack.apply(std::array{valtype::f32, valtype::f32}, std::array{valtype::i32}); Target::put_call(code, runtime::f32lt); nextop(); }
HANDLER(f64lt) {       stack.apply(std::array{valtype::f64, valtype::f64}, std::array{valtype::i32}); Target::put_call(code, runtime::f64lt); nextop(); }
HANDLER(f32gt) {       stack.apply(std::array{valtype::f32, valtype::f32}, std::array{valtype::i32}); Target::put_call(code, runtime::f32gt); nextop(); }
HANDLER(f64gt) {       stack.apply(std::array{valtype::f64, valtype::f64}, std::array{valtype::i32}); Target::put_call(code, runtime::f64gt); nextop(); }
HANDLER(f32le) {       stack.apply(std::array{valtype::f32, valtype::f32}, std::array{valtype::i32}); Target::put_call(code, runtime::f32le); nextop(); }
HANDLER(f64le) {       stack.apply(std::array{valtype::f64, valtype::f64}, std::array{valtype::i32}); Target::put_call(code, runtime::f64le); nextop(); }
HANDLER(f32ge) {       stack.apply(std::array{valtype::f32, valtype::f32}, std::array{valtype::i32}); Target::put_call(code, runtime::f32ge); nextop(); }
HANDLER(f64ge) {       stack.apply(std::array{valtype::f64, valtype::f64}, std::array{valtype::i32}); Target::put_call(code, runtime::f64ge); nextop(); }
HANDLER(i32clz) {      stack.apply(std::array{valtype::i32              }, std::array{valtype::i32}); Target::put_call(code, runtime::i32clz); nextop(); }
HANDLER(i64clz) {      stack.apply(std::array{valtype::i64              }, std::array{valtype::i64}); Target::put_call(code, runtime::i64clz); nextop(); }
HANDLER(i32ctz) {      stack.apply(std::array{valtype::i32              }, std::array{valtype::i32}); Target::put_call(code, runtime::i32ctz); nextop(); }
HANDLER(i64ctz) {      stack.apply(std::array{valtype::i64              }, std::array{valtype::i64}); Target::put_call(code, runtime::i64ctz); nextop(); }
HANDLER(i32popcnt) {   stack.apply(std::array{valtype::i32              }, std::array{valtype::i32}); Target::put_call(code, runtime::i32popcnt); nextop(); }
HANDLER(i64popcnt) {   stack.apply(std::array{valtype::i64              }, std::array{valtype::i64}); Target::put_call(code, runtime::i64popcnt); nextop(); }
HANDLER(i32add) {      stack.apply(std::array{valtype::i32, valtype::i32}, std::array{valtype::i32}); Target::put_call(code, runtime::i32add); nextop(); }
HANDLER(i64add) {      stack.apply(std::array{valtype::i64, valtype::i64}, std::array{valtype::i64}); Target::put_call(code, runtime::i64add); nextop(); }
HANDLER(i32sub) {      stack.apply(std::array{valtype::i32, valtype::i32}, std::array{valtype::i32}); Target::put_call(code, runtime::i32sub); nextop(); }
HANDLER(i64sub) {      stack.apply(std::array{valtype::i64, valtype::i64}, std::array{valtype::i64}); Target::put_call(code, runtime::i64sub); nextop(); }
HANDLER(i32mul) {      stack.apply(std::array{valtype::i32, valtype::i32}, std::array{valtype::i32}); Target::put_call(code, runtime::i32mul); nextop(); }
HANDLER(i64mul) {      stack.apply(std::array{valtype::i64, valtype::i64}, std::array{valtype::i64}); Target::put_call(code, runtime::i64mul); nextop(); }
HANDLER(i32div_s) {    stack.apply(std::array{valtype::i32, valtype::i32}, std::array{valtype::i32}); Target::put_call(code, runtime::i32div_s); nextop(); }
HANDLER(i64div_s) {    stack.apply(std::array{valtype::i64, valtype::i64}, std::array{valtype::i64}); Target::put_call(code, runtime::i64div_s); nextop(); }
HANDLER(i32div_u) {    stack.apply(std::array{valtype::i32, valtype::i32}, std::array{valtype::i32}); Target::put_call(code, runtime::i32div_u); nextop(); }
HANDLER(i64div_u) {    stack.apply(std::array{valtype::i64, valtype::i64}, std::array{valtype::i64}); Target::put_call(code, runtime::i64div_u); nextop(); }
HANDLER(i32rem_s) {    stack.apply(std::array{valtype::i32, valtype::i32}, std::array{valtype::i32}); Target::put_call(code, runtime::i32rem_s); nextop(); }
HANDLER(i64rem_s) {    stack.apply(std::array{valtype::i64, valtype::i64}, std::array{valtype::i64}); Target::put_call(code, runtime::i64rem_s); nextop(); }
HANDLER(i32rem_u) {    stack.apply(std::array{valtype::i32, valtype::i32}, std::array{valtype::i32}); Target::put_call(code, runtime::i32rem_u); nextop(); }
HANDLER(i64rem_u) {    stack.apply(std::array{valtype::i64, valtype::i64}, std::array{valtype::i64}); Target::put_call(code, runtime::i64rem_u); nextop(); }
HANDLER(i32and) {      stack.apply(std::array{valtype::i32, valtype::i32}, std::array{valtype::i32}); Target::put_call(code, runtime::i32and); nextop(); }
HANDLER(i64and) {      stack.apply(std::array{valtype::i64, valtype::i64}, std::array{valtype::i64}); Target::put_call(code, runtime::i64and); nextop(); }
HANDLER(i32or) {       stack.apply(std::array{valtype::i32, valtype::i32}, std::array{valtype::i32}); Target::put_call(code, runtime::i32or); nextop(); }
HANDLER(i64or) {       stack.apply(std::array{valtype::i64, valtype::i64}, std::array{valtype::i64}); Target::put_call(code, runtime::i64or); nextop(); }
HANDLER(i32xor) {      stack.apply(std::array{valtype::i32, valtype::i32}, std::array{valtype::i32}); Target::put_call(code, runtime::i32xor); nextop(); }
HANDLER(i64xor) {      stack.apply(std::array{valtype::i64, valtype::i64}, std::array{valtype::i64}); Target::put_call(code, runtime::i64xor); nextop(); }
HANDLER(i32shl) {      stack.apply(std::array{valtype::i32, valtype::i32}, std::array{valtype::i32}); Target::put_call(code, runtime::i32shl); nextop(); }
HANDLER(i64shl) {      stack.apply(std::array{valtype::i64, valtype::i64}, std::array{valtype::i64}); Target::put_call(code, runtime::i64shl); nextop(); }
HANDLER(i32shr_s) {    stack.apply(std::array{valtype::i32, valtype::i32}, std::array{valtype::i32}); Target::put_call(code, runtime::i32shr_s); nextop(); }
HANDLER(i64shr_s) {    stack.apply(std::array{valtype::i64, valtype::i64}, std::array{valtype::i64}); Target::put_call(code, runtime::i64shr_s); nextop(); }
HANDLER(i32shr_u) {    stack.apply(std::array{valtype::i32, valtype::i32}, std::array{valtype::i32}); Target::put_call(code, runtime::i32shr_u); nextop(); }
HANDLER(i64shr_u) {    stack.apply(std::array{valtype::i64, valtype::i64}, std::array{valtype::i64}); Target::put_call(code, runtime::i64shr_u); nextop(); }
HANDLER(i32rotl) {     stack.apply(std::array{valtype::i32, valtype::i32}, std::array{valtype::i32}); Target::put_call(code, runtime::i32rotl); nextop(); }
HANDLER(i64rotl) {     stack.apply(std::array{valtype::i64, valtype::i64}, std::array{valtype::i64}); Target::put_call(code, runtime::i64rotl); nextop(); }
HANDLER(i32rotr) {     stack.apply(std::array{valtype::i32, valtype::i32}, std::array{valtype::i32}); Target::put_call(code, runtime::i32rotr); nextop(); }
HANDLER(i64rotr) {     stack.apply(std::array{valtype::i64, valtype::i64}, std::array{valtype::i64}); Target::put_call(code, runtime::i64rotr); nextop(); }
HANDLER(f32abs) {      stack.apply(std::array{valtype::f32              }, std::array{valtype::f32}); Target::put_call(code, runtime::f32abs); nextop(); }
HANDLER(f64abs) {      stack.apply(std::array{valtype::f64              }, std::array{valtype::f64}); Target::put_call(code, runtime::f64abs); nextop(); }
HANDLER(f32neg) {      stack.apply(std::array{valtype::f32              }, std::array{valtype::f32}); Target::put_call(code, runtime::f32neg); nextop(); }
HANDLER(f64neg) {      stack.apply(std::array{valtype::f64              }, std::array{valtype::f64}); Target::put_call(code, runtime::f64neg); nextop(); }
HANDLER(f32ceil) {     stack.apply(std::array{valtype::f32              }, std::array{valtype::f32}); Target::put_call(code, runtime::f32ceil); nextop(); }
HANDLER(f64ceil) {     stack.apply(std::array{valtype::f64              }, std::array{valtype::f64}); Target::put_call(code, runtime::f64ceil); nextop(); }
HANDLER(f32floor) {    stack.apply(std::array{valtype::f32              }, std::array{valtype::f32}); Target::put_call(code, runtime::f32floor); nextop(); }
HANDLER(f64floor) {    stack.apply(std::array{valtype::f64              }, std::array{valtype::f64}); Target::put_call(code, runtime::f64floor); nextop(); }
HANDLER(f32trunc) {    stack.apply(std::array{valtype::f32              }, std::array{valtype::f32}); Target::put_call(code, runtime::f32trunc); nextop(); }
HANDLER(f64trunc) {    stack.apply(std::array{valtype::f64              }, std::array{valtype::f64}); Target::put_call(code, runtime::f64trunc); nextop(); }
HANDLER(f32nearest) {  stack.apply(std::array{valtype::f32              }, std::array{valtype::f32}); Target::put_call(code, runtime::f32nearest); nextop(); }
HANDLER(f64nearest) {  stack.apply(std::array{valtype::f64              }, std::array{valtype::f64}); Target::put_call(code, runtime::f64nearest); nextop(); }
HANDLER(f32sqrt) {     stack.apply(std::array{valtype::f32              }, std::array{valtype::f32}); Target::put_call(code, runtime::f32sqrt); nextop(); }
HANDLER(f64sqrt) {     stack.apply(std::array{valtype::f64              }, std::array{valtype::f64}); Target::put_call(code, runtime::f64sqrt); nextop(); }
HANDLER(f32add) {      stack.apply(std::array{valtype::f32, valtype::f32}, std::array{valtype::f32}); Target::put_call(code, runtime::f32add); nextop(); }
HANDLER(f64add) {      stack.apply(std::array{valtype::f64, valtype::f64}, std::array{valtype::f64}); Target::put_call(code, runtime::f64add); nextop(); }
HANDLER(f32sub) {      stack.apply(std::array{valtype::f32, valtype::f32}, std::array{valtype::f32}); Target::put_call(code, runtime::f32sub); nextop(); }
HANDLER(f64sub) {      stack.apply(std::array{valtype::f64, valtype::f64}, std::array{valtype::f64}); Target::put_call(code, runtime::f64sub); nextop(); }
HANDLER(f32mul) {      stack.apply(std::array{valtype::f32, valtype::f32}, std::array{valtype::f32}); Target::put_call(code, runtime::f32mul); nextop(); }
HANDLER(f64mul) {      stack.apply(std::array{valtype::f64, valtype::f64}, std::array{valtype::f64}); Target::put_call(code, runtime::f64mul); nextop(); }
HANDLER(f32div) {      stack.apply(std::array{valtype::f32, valtype::f32}, std::array{valtype::f32}); Target::put_call(code, runtime::f32div); nextop(); }
HANDLER(f64div) {      stack.apply(std::array{valtype::f64, valtype::f64}, std::array{valtype::f64}); Target::put_call(code, runtime::f64div); nextop(); }
HANDLER(f32min) {      stack.apply(std::array{valtype::f32, valtype::f32}, std::array{valtype::f32}); Target::put_call(code, runtime::f32min); nextop(); }
HANDLER(f64min) {      stack.apply(std::array{valtype::f64, valtype::f64}, std::array{valtype::f64}); Target::put_call(code, runtime::f64min); nextop(); }
HANDLER(f32max) {      stack.apply(std::array{valtype::f32, valtype::f32}, std::array{valtype::f32}); Target::put_call(code, runtime::f32max); nextop(); }
HANDLER(f64max) {      stack.apply(std::array{valtype::f64, valtype::f64}, std::array{valtype::f64}); Target::put_call(code, runtime::f64max); nextop(); }
HANDLER(f32copysign) { stack.apply(std::array{valtype::f32, valtype::f32}, std::array{valtype::f32}); Target::put_call(code, runtime::f32copysign); nextop(); }
HANDLER(f64copysign) { stack.apply(std::array{valtype::f64, valtype::f64}, std::array{valtype::f64}); Target::put_call(code, runtime::f64copysign); nextop(); }
HANDLER(i32wrap_i64) {      stack.apply(std::array{valtype::i64}, std::array{valtype::i32}); Target::put_call(code, runtime::i32wrap_i64); nextop(); }
HANDLER(i64extend_i32_s) {  stack.apply(std::array{valtype::i32}, std::array{valtype::i64}); Target::put_call(code, runtime::i64extend_i32_s); nextop(); }
HANDLER(i64extend_i32_u) {  stack.apply(std::array{valtype::i32}, std::array{valtype::i64}); Target::put_call(code, runtime::i64extend_i32_u); nextop(); }
HANDLER(i32trunc_f32_s) {   stack.apply(std::array{valtype::f32}, std::array{valtype::i32}); Target::put_call(code, runtime::i32trunc_f32_s); nextop(); }
HANDLER(i64trunc_f32_s) {   stack.apply(std::array{valtype::f32}, std::array{valtype::i64}); Target::put_call(code, runtime::i64trunc_f32_s); nextop(); }
HANDLER(i32trunc_f32_u) {   stack.apply(std::array{valtype::f32}, std::array{valtype::i32}); Target::put_call(code, runtime::i32trunc_f32_u); nextop(); }
HANDLER(i64trunc_f32_u) {   stack.apply(std::array{valtype::f32}, std::array{valtype::i64}); Target::put_call(code, runtime::i64trunc_f32_u); nextop(); }
HANDLER(i32trunc_f64_s) {   stack.apply(std::array{valtype::f64}, std::array{valtype::i32}); Target::put_call(code, runtime::i32trunc_f64_s); nextop(); }
HANDLER(i64trunc_f64_s) {   stack.apply(std::array{valtype::f64}, std::array{valtype::i64}); Target::put_call(code, runtime::i64trunc_f64_s); nextop(); }
HANDLER(i32trunc_f64_u) {   stack.apply(std::array{valtype::f64}, std::array{valtype::i32}); Target::put_call(code, runtime::i32trunc_f64_u); nextop(); }
HANDLER(i64trunc_f64_u) {   stack.apply(std::array{valtype::f64}, std::array{valtype::i64}); Target::put_call(code, runtime::i64trunc_f64_u); nextop(); }
HANDLER(f32convert_i32_s) { stack.apply(std::array{valtype::i32}, std::array{valtype::f32}); Target::put_call(code, runtime::f32convert_i32_s); nextop(); }
HANDLER(f64convert_i32_s) { stack.apply(std::array{valtype::i32}, std::array{valtype::f64}); Target::put_call(code, runtime::f64convert_i32_s); nextop(); }
HANDLER(f32convert_i32_u) { stack.apply(std::array{valtype::i32}, std::array{valtype::f32}); Target::put_call(code, runtime::f32convert_i32_u); nextop(); }
HANDLER(f64convert_i32_u) { stack.apply(std::array{valtype::i32}, std::array{valtype::f64}); Target::put_call(code, runtime::f64convert_i32_u); nextop(); }
HANDLER(f32convert_i64_s) { stack.apply(std::array{valtype::i64}, std::array{valtype::f32}); Target::put_call(code, runtime::f32convert_i64_s); nextop(); }
HANDLER(f64convert_i64_s) { stack.apply(std::array{valtype::i64}, std::array{valtype::f64}); Target::put_call(code, runtime::f64convert_i64_s); nextop(); }
HANDLER(f32convert_i64_u) { stack.apply(std::array{valtype::i64}, std::array{valtype::f32}); Target::put_call(code, runtime::f32convert_i64_u); nextop(); }
HANDLER(f64convert_i64_u) { stack.apply(std::array{valtype::i64}, std::array{valtype::f64}); Target::put_call(code, runtime::f64convert_i64_u); nextop(); }
HANDLER(f32demote_f64) {    stack.apply(std::array{valtype::f64}, std::array{valtype::f32}); Target::put_call(code, runtime::f32demote_f64); nextop(); }
HANDLER(f64promote_f32) {   stack.apply(std::array{valtype::f32}, std::array{valtype::f64}); Target::put_call(code, runtime::f64promote_f32); nextop(); }
HANDLER(i32reinterpret_f32) { stack.apply(std::array{valtype::f32}, std::array{valtype::i32}); nextop(); }
HANDLER(f32reinterpret_i32) { stack.apply(std::array{valtype::i32}, std::array{valtype::f32}); nextop(); }
HANDLER(i64reinterpret_f64) { stack.apply(std::array{valtype::f64}, std::array{valtype::i64}); nextop(); }
HANDLER(f64reinterpret_i64) { stack.apply(std::array{valtype::i64}, std::array{valtype::f64}); nextop(); }
HANDLER(i32extend8_s) {  stack.apply(std::array{valtype::i32}, std::array{valtype::i32}); Target::put_call(code, runtime::i32extend8_s); nextop(); }
HANDLER(i32extend16_s) { stack.apply(std::array{valtype::i32}, std::array{valtype::i32}); Target::put_call(code, runtime::i32extend16_s); nextop(); }
HANDLER(i64extend8_s) {  stack.apply(std::array{valtype::i64}, std::array{valtype::i64}); Target::put_call(code, runtime::i64extend8_s); nextop(); }
HANDLER(i64extend16_s) { stack.apply(std::array{valtype::i64}, std::array{valtype::i64}); Target::put_call(code, runtime::i64extend16_s); nextop(); }
HANDLER(i64extend32_s) { stack.apply(std::array{valtype::i64}, std::array{valtype::i64}); Target::put_call(code, runtime::i64extend32_s); nextop(); }
// clang-format on
HANDLER(ref_null) {
    auto type_idx = safe_read_leb128<uint32_t>(iter);
    ensure(is_reftype(type_idx), "type mismatch");
    stack.apply(std::array<valtype, 0>(),
                std::array{static_cast<valtype>(type_idx)});

    Target::put_call(code, runtime::ref_null);
    nextop();
}
HANDLER(ref_is_null) {
    auto peek = stack.back();
    ensure(peek == valtype::any || is_reftype(peek), "type mismatch");
    stack.apply(std::array{peek}, std::array{valtype::i32});

    Target::put_call(code, runtime::ref_is_null);
    nextop();
}
HANDLER(ref_func) {
    auto func_idx = safe_read_leb128<uint32_t>(iter);
    ensure(func_idx < mod.functions.size(), "invalid function index");
    ensure(mod.functions[func_idx].is_declared,
           "undeclared function reference");
    stack.apply(std::array<valtype, 0>(), std::array{valtype::funcref});

    Target::put_temp1(code, 1 + func_idx);
    Target::put_call(code, runtime::ref_func);
    nextop();
}
HANDLER(ref_eq) {
    auto peek = stack.back();
    ensure(peek == valtype::any || is_reftype(peek), "type mismatch");
    stack.apply(std::array{peek, peek}, std::array{valtype::i32});

    Target::put_call(code, runtime::ref_eq);
    nextop();
}
// clang-format off
HANDLER(i32_trunc_sat_f32_s) { stack.apply(std::array{valtype::f32}, std::array{valtype::i32}); Target::put_call(code, runtime::i32_trunc_sat_f32_s); nextop(); }
HANDLER(i32_trunc_sat_f32_u) { stack.apply(std::array{valtype::f32}, std::array{valtype::i32}); Target::put_call(code, runtime::i32_trunc_sat_f32_u); nextop(); }
HANDLER(i32_trunc_sat_f64_s) { stack.apply(std::array{valtype::f64}, std::array{valtype::i32}); Target::put_call(code, runtime::i32_trunc_sat_f64_s); nextop(); }
HANDLER(i32_trunc_sat_f64_u) { stack.apply(std::array{valtype::f64}, std::array{valtype::i32}); Target::put_call(code, runtime::i32_trunc_sat_f64_u); nextop(); }
HANDLER(i64_trunc_sat_f32_s) { stack.apply(std::array{valtype::f32}, std::array{valtype::i64}); Target::put_call(code, runtime::i64_trunc_sat_f32_s); nextop(); }
HANDLER(i64_trunc_sat_f32_u) { stack.apply(std::array{valtype::f32}, std::array{valtype::i64}); Target::put_call(code, runtime::i64_trunc_sat_f32_u); nextop(); }
HANDLER(i64_trunc_sat_f64_s) { stack.apply(std::array{valtype::f64}, std::array{valtype::i64}); Target::put_call(code, runtime::i64_trunc_sat_f64_s); nextop(); }
HANDLER(i64_trunc_sat_f64_u) { stack.apply(std::array{valtype::f64}, std::array{valtype::i64}); Target::put_call(code, runtime::i64_trunc_sat_f64_u); nextop(); }
// clang-format on
HANDLER(memory_init) {
    auto seg_idx = safe_read_leb128<uint32_t>(iter);
    if (*iter++ != 0)
        error<malformed_error>("zero byte expected");

    ensure(mod.memory.exists, "unknown memory 0");
    if (mod.n_data == std::numeric_limits<uint32_t>::max()) {
        error<malformed_error>("data count section required");
    }
    ensure(seg_idx < mod.n_data, "unknown data segment");

    stack.apply(std::array{valtype::i32, valtype::i32, valtype::i32},
                std::array<valtype, 0>());

    // todo: in theory this could be optimized to directly give address
    // since data segments are shared and known at this point,
    // but data section comes after code section so stuff would have to move
    Target::put_temp1(code, 1 + mod.functions.size() + mod.tables.size() +
                                mod.globals.size() + mod.elements.size() +
                                seg_idx);
    Target::put_call(code, runtime::memory_init);
    nextop();
}
HANDLER(data_drop) {
    auto seg_idx = safe_read_leb128<uint32_t>(iter);
    if (mod.n_data == std::numeric_limits<uint32_t>::max()) {
        error<malformed_error>("data count section required");
    }
    ensure(seg_idx < mod.n_data, "unknown data segment");
    Target::put_temp1(code, 1 + mod.functions.size() + mod.tables.size() +
                                mod.globals.size() + mod.elements.size() +
                                seg_idx);
    Target::put_call(code, runtime::data_drop);
    nextop();
}
HANDLER(memory_copy) {
    if (*iter++ != 0)
        error<malformed_error>("zero byte expected");
    if (*iter++ != 0)
        error<malformed_error>("zero byte expected");
    ensure(mod.memory.exists, "unknown memory 0");

    stack.apply(std::array{valtype::i32, valtype::i32, valtype::i32},
                std::array<valtype, 0>());

    Target::put_call(code, runtime::memory_copy);
    nextop();
}
HANDLER(memory_fill) {
    if (*iter++ != 0)
        error<malformed_error>("zero byte expected");
    ensure(mod.memory.exists, "unknown memory 0");

    stack.apply(std::array{valtype::i32, valtype::i32, valtype::i32},
                std::array<valtype, 0>());

    Target::put_call(code, runtime::memory_fill);
    nextop();
}
HANDLER(table_init) {
    auto seg_idx = safe_read_leb128<uint32_t>(iter);
    auto table_idx = safe_read_leb128<uint32_t>(iter);

    ensure(table_idx < mod.tables.size(), "unknown table");
    ensure(seg_idx < mod.elements.size(), "unknown data segment");
    ensure(mod.tables[table_idx].type == mod.elements[seg_idx].type,
           "type mismatch");

    stack.apply(std::array{valtype::i32, valtype::i32, valtype::i32},
                std::array<valtype, 0>());

    Target::put_temp1(code, 1 + mod.functions.size() + mod.tables.size() +
                                mod.globals.size() + seg_idx);
    Target::put_temp2(code, 1 + mod.functions.size() + table_idx);
    Target::put_call(code, runtime::table_init);
    nextop();
}
HANDLER(elem_drop) {
    auto seg_idx = safe_read_leb128<uint32_t>(iter);
    ensure(seg_idx < mod.elements.size(), "unknown elem segment");

    Target::put_temp1(code, 1 + mod.functions.size() + mod.tables.size() +
                                mod.globals.size() + seg_idx);
    Target::put_call(code, runtime::elem_drop);
    nextop();
}
HANDLER(table_copy) {
    auto dst_table_idx = safe_read_leb128<uint32_t>(iter);
    ensure(dst_table_idx < mod.tables.size(), "unknown table");
    auto src_table_idx = safe_read_leb128<uint32_t>(iter);
    ensure(src_table_idx < mod.tables.size(), "unknown table");
    ensure(mod.tables[src_table_idx].type == mod.tables[dst_table_idx].type,
           "type mismatch");

    stack.apply(std::array{valtype::i32, valtype::i32, valtype::i32},
                std::array<valtype, 0>());

    Target::put_temp1(code, 1 + mod.functions.size() + dst_table_idx);
    Target::put_temp2(code, 1 + mod.functions.size() + src_table_idx);
    Target::put_call(code, runtime::table_copy);
    nextop();
}
HANDLER(table_grow) {
    auto table_idx = safe_read_leb128<uint32_t>(iter);
    ensure(table_idx < mod.tables.size(), "unknown table");

    stack.apply(std::array{mod.tables[table_idx].type, valtype::i32},
                std::array{valtype::i32});

    Target::put_temp1(code, 1 + mod.functions.size() + table_idx);
    Target::put_call(code, runtime::table_grow);
    nextop();
}
HANDLER(table_size) {
    auto table_idx = safe_read_leb128<uint32_t>(iter);
    ensure(table_idx < mod.tables.size(), "unknown table");

    stack.apply(std::array<valtype, 0>(), std::array{valtype::i32});

    Target::put_temp1(code, 1 + mod.functions.size() + table_idx);
    Target::put_call(code, runtime::table_size);
    nextop();
}
HANDLER(table_fill) {
    auto table_idx = safe_read_leb128<uint32_t>(iter);
    ensure(table_idx < mod.tables.size(), "unknown table");

    stack.apply(
        std::array{valtype::i32, mod.tables[table_idx].type, valtype::i32},
        std::array<valtype, 0>());

    Target::put_temp1(code, 1 + mod.functions.size() + table_idx);
    Target::put_call(code, runtime::table_fill);
    nextop();
}

template <typename Target>
consteval std::array<CompilationHandler *, 256> make_fc_funcs() {
    std::array<CompilationHandler *, 256> fc_funcs;
    fc_funcs.fill(validate_missing<Target>);

#define V(name, _, byte) fc_funcs[byte] = &validate_##name<Target>;
    FOREACH_MULTIBYTE_INSTRUCTION(V)
#undef V

    return fc_funcs;
}

HANDLER(multibyte) {
    constexpr auto fc_funcs = make_fc_funcs<Target>();

    auto byte = safe_read_leb128<uint8_t, 32>(iter);
    [[clang::musttail]] return fc_funcs[byte](mod, iter, fn, stack,
                                              control_stack, code);
}

template <typename Pager, typename Target>
uint8_t *Module::validate_and_compile(safe_byte_iterator &iter, uint8_t *code,
                                      FunctionShell &fn) {
    auto stack = WasmStack();

    auto control_stack = std::vector<ControlFlow>(
        {ControlFlow(fn.type.results, {}, {}, fn.type, false, 0, Function())});

    auto locals_bytes = fn.locals.bytesize() - fn.type.params.bytesize();

    Target::put_prelude(code);
    Target::put_temp1(code, locals_bytes);
    Target::put_call(code, runtime::clear_locals);

    auto byte = *iter++;
    return funcs<Target>[byte](*this, iter, fn, stack, control_stack, code);
}

#undef LOAD
#undef STORE
#undef HANDLER
#undef nextop

#pragma clang diagnostic pop

} // namespace mitey