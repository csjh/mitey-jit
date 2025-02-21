#include "instance.hpp"
#include "./module.hpp"
#include "interfacing.hpp"
#include "runtime.hpp"
#include "spec.hpp"

namespace mitey {

auto Instance::initial_stack = Allocation(nullptr, [](auto) {});

Instance::Instance(std::shared_ptr<Module> module)
    : module(module), misc(std::make_unique<void *[]>(
                          1 + module->functions.size() + module->tables.size() +
                          module->globals.size() + module->elements.size() +
                          module->data_segments.size())),
      memory(nullptr), functions(module->functions.size()),
      globals(module->globals.size()), elements(module->elements.size()),
      tables(module->tables.size()) {}

void Instance::initialize(const runtime::Imports &imports) {
    auto prev = runtime::trap_buf;
    std::jmp_buf buf;
    runtime::trap_buf = &buf;
    auto result = static_cast<runtime::TrapKind>(setjmp(*runtime::trap_buf));
    if (result != runtime::TrapKind::success) {
        error<uninstantiable_error>(runtime::trap_kind_to_string(result));
    }

    void **misc_ptr = misc.get();
    auto misc_memory = reinterpret_cast<runtime::WasmMemory **>(misc_ptr);
    // only used for imported functions
    // good opportunity for tiering up though
    auto misc_functions = reinterpret_cast<runtime::Funcref *>(misc_ptr += 1);
    auto misc_tables = reinterpret_cast<runtime::WasmTable **>(
        misc_ptr += module->functions.size());
    auto misc_globals = reinterpret_cast<runtime::WasmValue **>(
        misc_ptr += module->tables.size());
    auto misc_elements = reinterpret_cast<runtime::ElementSegment **>(
        misc_ptr += module->globals.size());
    auto misc_segments = reinterpret_cast<runtime::Segment **>(
        misc_ptr += module->elements.size());

    auto get_import = [&](const ImportSpecifier &specifier) {
        auto [module_name, field_name] = specifier;
        if (!imports.contains(module_name)) {
            error<link_error>("unknown import");
        }
        auto &import_module = imports.at(module_name);
        if (!import_module.contains(field_name)) {
            error<link_error>("unknown import");
        }
        auto &import = import_module.at(field_name);
        if (static_cast<mitey::ImExDesc>(import.index()) !=
            module->imports.at(module_name).at(field_name)) {
            error<link_error>("incompatible import type: descriptor incorrect");
        }
        return import;
    };

    if (!module->memory.exists) {
        memory = std::make_shared<runtime::WasmMemory>();
    } else if (module->memory.import) {
        auto imported_memory = std::get<std::shared_ptr<runtime::WasmMemory>>(
            get_import(*module->memory.import));

        if (imported_memory->size() < module->memory.min ||
            imported_memory->max() > module->memory.max) {
            error<link_error>(
                "incompatible import type: memory size doesn't fit");
        }

        memory = imported_memory;
    } else {
        memory = std::make_shared<runtime::WasmMemory>(module->memory.min,
                                                       module->memory.max);
    }
    *misc_memory = memory.get();

    for (uint32_t i = 0; i < functions.size(); i++) {
        const auto &fn = module->functions[i];
        if (fn.import) {
            auto imported_function =
                std::get<runtime::FunctionInfo>(get_import(*fn.import));

            if (imported_function.type != runtime::FunctionType(fn.type)) {
                error<link_error>(
                    "incompatible import type: function type doesn't match");
            }

            functions[i] = imported_function;
            if (!functions[i].instance) {
                functions[i].memory = memory.get()->memory.get();
                functions[i].misc = misc.get();
                functions[i].instance = self.lock();
            }
        } else {
            auto memory = this->memory.get()->memory.get();
            auto misc = this->misc.get();
            functions[i] = runtime::FunctionInfo(
                runtime::FunctionType(fn.type), memory, misc,
                reinterpret_cast<runtime::TemplessSignature *>(fn.start),
                self.lock());
        }
        misc_functions[i] = &functions[i];
    }

    for (uint32_t i = 0; i < globals.size(); i++) {
        const auto &global = module->globals[i];
        if (global.import) {
            auto imported_global =
                std::get<std::shared_ptr<runtime::WasmGlobal>>(
                    get_import(*global.import));

            if (imported_global->type != global.type ||
                imported_global->_mut != global.mutability) {
                error<link_error>("incompatible import type: global mutability "
                                  "doesn't match");
            }

            globals[i] = imported_global;
        } else {
            globals[i] = std::make_shared<runtime::WasmGlobal>(
                global.type, global.mutability,
                interpret_const_inplace(global.initializer));
        }
        misc_globals[i] = &(globals[i].get()->value);
    }

    for (uint32_t i = 0; i < tables.size(); i++) {
        const auto &table = module->tables[i];
        if (table.import) {
            auto imported_table = std::get<std::shared_ptr<runtime::WasmTable>>(
                get_import(*table.import));

            if (imported_table->size() < table.min ||
                imported_table->max() > table.max ||
                imported_table->type != table.type) {
                error<link_error>(
                    "incompatible import type: table size doesn't fit");
            }

            tables[i] = imported_table;
        } else {
            tables[i] = std::make_shared<runtime::WasmTable>(
                table.type, table.min, table.max);
        }
        misc_tables[i] = tables[i].get();
    }

    auto iter = module->element_start;
    for (uint32_t i = 0; i < elements.size(); i++) {
        auto flags = safe_read_leb128<uint32_t>(iter);

        if (flags & 1) {
            if (flags & 0b10) {
                if (flags & 0b100) {
                    // flags = 7
                    // characteristics: declarative, elem type + exprs
                    auto reftype = static_cast<valtype>(*iter++);
                    auto n_elements = safe_read_leb128<uint32_t>(iter);
                    for (uint32_t j = 0; j < n_elements; j++) {
                        interpret_const(iter);
                    }
                    elements[i] = runtime::ElementSegment(reftype, 0, nullptr);
                } else {
                    // flags = 3
                    // characteristics: declarative, elem kind + indices
                    /* uint8_t elemkind = * */ iter++;
                    auto n_elements = safe_read_leb128<uint32_t>(iter);
                    for (uint32_t j = 0; j < n_elements; j++) {
                        safe_read_leb128<uint32_t>(iter);
                    }
                    elements[i] =
                        runtime::ElementSegment(valtype::funcref, 0, nullptr);
                }
            } else {
                if (flags & 0b100) {
                    // flags = 5
                    // characteristics: passive, elem type + exprs
                    auto reftype = static_cast<valtype>(*iter++);
                    auto n_elements = safe_read_leb128<uint32_t>(iter);
                    auto elem =
                        std::make_unique<runtime::WasmValue[]>(n_elements);
                    for (uint32_t j = 0; j < n_elements; j++) {
                        elem[j] = interpret_const(iter);
                    }
                    elements[i] = runtime::ElementSegment(reftype, n_elements,
                                                          std::move(elem));
                } else {
                    // flags = 1
                    // characteristics: passive, elem kind + indices
                    /* uint8_t elemkind = * */ iter++;
                    auto n_elements = safe_read_leb128<uint32_t>(iter);
                    auto elem =
                        std::make_unique<runtime::WasmValue[]>(n_elements);
                    for (uint32_t j = 0; j < n_elements; j++) {
                        elem[j] = &functions[safe_read_leb128<uint32_t>(iter)];
                    }
                    elements[i] = runtime::ElementSegment(
                        valtype::funcref, n_elements, std::move(elem));
                }
            }
        } else {
            auto reftype = valtype::null;

            auto table =
                tables[flags & 0b10 ? safe_read_leb128<uint32_t>(iter) : 0];
            auto offset = interpret_const(iter).u32;
            auto reftype_or_elemkind = flags & 0b10 ? *iter++ : 256;
            auto n_elements = safe_read_leb128<uint32_t>(iter);

            if (offset + n_elements > table->size()) {
                error<uninstantiable_error>("out of bounds table access");
            }

            if (flags & 0b100) {
                // flags = 4 or 6
                // characteristics: active, elem type + exprs
                if (reftype_or_elemkind == 256)
                    reftype_or_elemkind =
                        static_cast<uint16_t>(valtype::funcref);
                reftype = static_cast<valtype>(reftype_or_elemkind);

                for (uint32_t j = 0; j < n_elements; j++) {
                    auto ref = interpret_const(iter);
                    table->set(offset + j, ref);
                }
            } else {
                if (reftype_or_elemkind == 256)
                    reftype_or_elemkind = 0;
                reftype = valtype::funcref;

                // flags = 0 or 2
                // characteristics: active, elem kind + indices
                for (uint32_t j = 0; j < n_elements; j++) {
                    auto elem_idx = safe_read_leb128<uint32_t>(iter);
                    auto funcref = &functions[elem_idx];
                    table->set(offset + j, funcref);
                }
            }
            elements[i] = runtime::ElementSegment(reftype, 0, nullptr);
        }

        misc_elements[i] = &elements[i];
    }

    for (uint32_t i = 0; i < module->data_segments.size(); i++) {
        auto &data = module->data_segments[i];
        if (data.initializer) {
            auto offset = interpret_const_inplace(data.initializer).u32;

            memory->copy_into(offset, 0, data, data.size);

            misc_segments[i] = &runtime::Segment::empty;
        } else {
            misc_segments[i] = &data;
        }
    }

    for (const auto &[name, export_] : module->exports) {
        switch (export_.desc) {
        case ImExDesc::func:
            exports.insert({name, functions[export_.idx]});
            break;
        case ImExDesc::table:
            exports.insert({name, tables[export_.idx]});
            break;
        case ImExDesc::mem:
            exports.insert({name, memory});
            break;
        case ImExDesc::global:
            exports.insert({name, globals[export_.idx]});
            break;
        }
    }

    if (module->start != std::numeric_limits<uint32_t>::max()) {
        const auto &fn = functions[module->start];
        if (fn.type.params || fn.type.results) {
            error<validation_error>("start function");
        }
        auto stack =
            reinterpret_cast<runtime::WasmValue *>(initial_stack.get());
        fn.signature(fn.memory, fn.misc, stack);
    }

    runtime::trap_buf = prev;
}

runtime::WasmValue Instance::interpret_const(uint8_t *&iter) {
    auto stack = std::vector<runtime::WasmValue>();

#define OP(ty, op)                                                             \
    {                                                                          \
        auto arg1 = stack.back();                                              \
        stack.pop_back();                                                      \
        auto arg2 = stack.back();                                              \
        stack.pop_back();                                                      \
        stack.push_back(arg1.ty op arg2.ty);                                   \
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
            stack.push_back(safe_read_sleb128<int32_t>(iter));
            break;
        case i64const:
            stack.push_back(safe_read_sleb128<int64_t>(iter));
            break;
        case f32const: {
            float x;
            std::memcpy(&x, iter, sizeof(float));
            stack.push_back(x);
            iter += sizeof(float);
            break;
        }
        case f64const: {
            double x;
            std::memcpy(&x, iter, sizeof(double));
            stack.push_back(x);
            iter += sizeof(double);
            break;
        }
        case globalget:
            stack.push_back(globals[safe_read_leb128<int32_t>(iter)]->value);
            break;
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
            safe_read_leb128<uint32_t>(iter);
            stack.push_back(Externref(nullptr));
            break;
        }
        case ref_func: {
            uint32_t func_idx = safe_read_leb128<uint32_t>(iter);
            stack.push_back(&functions[func_idx]);
            break;
        }
        default:
            __builtin_unreachable();
        }
    }

    return stack.back();

#undef OP
#undef I32_OP
#undef I64_OP
}

} // namespace mitey