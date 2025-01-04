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
        auto stack = Instance::initial_stack.get();

        push_tuple_to_wasm(std::make_tuple(args...), stack,
                           std::make_index_sequence<Traits::parameter_arity>{});

        uint64_t a, b;
        fn.signature(fn.instance->memory.get(), fn.instance->misc.get(),
                     stack + Traits::parameter_arity, a, b);

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
            trap("invalid number of arguments");
        }

        auto stack = Instance::initial_stack.get();
        std::copy(args.begin(), args.end(), stack);

        uint64_t a, b;
        fn.signature(fn.instance->memory.get(), fn.instance->misc.get(), stack,
                     a, b);

        return std::vector<runtime::WasmValue>(stack, stack + fn.type.results);
    };
}

template <typename F, typename Callable>
void call_with_stack(Callable &&func, runtime::WasmValue *stack) {
    using Fn = function_traits<F>;
    using Args = typename Fn::args;
    using ReturnType = typename Fn::return_type;

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
}

template <auto func> runtime::Signature *wasm_functionify() {
    return [](runtime::WasmMemory *memory, void **misc,
              runtime::WasmValue *stack, uint64_t tmp1,
              uint64_t tmp2) { call_with_stack<decltype(func)>(func, stack); };
}

template <auto func> runtime::FunctionInfo internalize() {
    return runtime::FunctionInfo(WasmSignature::from_type<decltype(func)>(),
                                 nullptr, wasm_functionify<func>());
}

} // namespace mitey
