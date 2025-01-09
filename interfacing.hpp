#include "instance.hpp"
#include "runtime.hpp"
#include "type-templates.hpp"

namespace mitey {

template <typename Tuple, size_t... I>
void push_tuple_to_wasm(const Tuple &t, runtime::WasmValue *out,
                        std::index_sequence<I...>) {
    ((out[I] = std::get<I>(t)), ...);
}

template <typename FunctionType>
std::function<FunctionType> externalize(const runtime::FunctionInfo &fn) {
    using Traits = function_traits<FunctionType *>;
    using ReturnType = typename Traits::return_type;

    return [=](auto... args) {
        auto stack = reinterpret_cast<runtime::WasmValue *>(
            Instance::initial_stack.get());

        push_tuple_to_wasm(std::make_tuple(args...), stack,
                           std::make_index_sequence<Traits::parameter_arity>{});

        auto prev_depth = runtime::call_stack_depth;
        auto prev_buf = runtime::trap_buf;
        std::jmp_buf buf;
        runtime::trap_buf = &buf;
        auto result =
            static_cast<runtime::TrapKind>(setjmp(*runtime::trap_buf));
        if (result != runtime::TrapKind::success) {
            runtime::trap_buf = prev_buf;
            runtime::call_stack_depth = prev_depth;
            error<trap_error>(runtime::trap_kind_to_string(result));
        }

        fn.signature(fn.memory, fn.misc, stack + Traits::parameter_arity, 0, 0);

        runtime::trap_buf = prev_buf;
        runtime::call_stack_depth = prev_depth;

        constexpr auto arity = Traits::result_arity;
        if constexpr (arity == 0) {
            return;
        } else if constexpr (arity == 1) {
            return stack[0];
        } else {
            return [&]<size_t... I>(std::index_sequence<I...>) {
                return ReturnType{(stack[I])...};
            }(std::make_index_sequence<arity>{});
        }
    };
}

inline std::function<
    std::vector<runtime::WasmValue>(const std::vector<runtime::WasmValue> &)>
externalize(const runtime::FunctionInfo &fn) {
    return [=](const std::vector<runtime::WasmValue> &args) {
        if (args.size() != fn.type.params) {
            error<trap_error>("invalid number of arguments");
        }

        auto stack = reinterpret_cast<runtime::WasmValue *>(
            Instance::initial_stack.get());
        std::copy(args.begin(), args.end(), stack);

        auto prev_depth = runtime::call_stack_depth;
        auto prev_buf = runtime::trap_buf;
        std::jmp_buf buf;
        runtime::trap_buf = &buf;
        auto result =
            static_cast<runtime::TrapKind>(setjmp(*runtime::trap_buf));
        if (result != runtime::TrapKind::success) {
            runtime::trap_buf = prev_buf;
            runtime::call_stack_depth = prev_depth;
            error<trap_error>(runtime::trap_kind_to_string(result));
        }

        fn.signature(fn.memory, fn.misc, stack + args.size(), 0, 0);

        runtime::trap_buf = prev_buf;
        runtime::call_stack_depth = prev_depth;

        return std::vector<runtime::WasmValue>(stack, stack + fn.type.results);
    };
}

template <auto func> runtime::Signature *wasm_functionify() {
    return [](runtime::WasmMemory *memory, void **misc,
              runtime::WasmValue *stack, uint64_t, uint64_t) {
        using Fn = function_traits<decltype(func)>;
        using Args = typename Fn::args;

        // Convert input arguments to tuple
        auto args = [&]<size_t... I>(std::index_sequence<I...>) {
            return Args{(stack[I])...};
        }(std::make_index_sequence<Fn::parameter_arity>{});

        constexpr auto arity = Fn::result_arity;
        if constexpr (arity == 0) {
            std::apply(func, args);
        } else if constexpr (arity == 1) {
            *stack = std::apply(func, args);
        } else {
            auto ret = std::apply(func, args);
            push_tuple_to_wasm(ret, stack, std::make_index_sequence<arity>{});
        }
        stack += arity;

        return runtime::dummy(memory, misc, stack);
    };
}

template <auto func> runtime::FunctionInfo internalize() {
    static void *misc[] = {&runtime::WasmMemory::empty};
    return runtime::FunctionInfo(WasmSignature::from_type<decltype(func)>(),
                                 &runtime::WasmMemory::empty, misc,
                                 wasm_functionify<func>(), nullptr);
}

} // namespace mitey
