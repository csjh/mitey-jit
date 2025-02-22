#include "instance.hpp"
#include "runtime.hpp"
#include "type-templates.hpp"
#include <tuple>

namespace mitey {

template <typename Tuple, size_t... I>
void push_tuple_to_wasm(const Tuple &t, runtime::WasmValue *out,
                        std::index_sequence<I...>) {
    ((out[I] = std::get<I>(t)), ...);
}

template <typename T> T normalize(std::byte *memory, runtime::WasmValue value) {
    if constexpr (std::is_pointer_v<T>) {
        return T(memory + value.u32);
    } else {
        return T(value);
    }
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

        fn.signature(fn.memory, fn.misc, stack + Traits::parameter_arity);

        runtime::trap_buf = prev_buf;
        runtime::call_stack_depth = prev_depth;

        constexpr auto arity = Traits::result_arity;
        if constexpr (arity == 0) {
            return;
        } else if constexpr (arity == 1) {
            return normalize<ReturnType>(fn.memory, stack[0]);
        } else {
            return [&]<size_t... I>(std::index_sequence<I...>) {
                return ReturnType{
                    normalize<std::tuple_element_t<I, ReturnType>>(
                        fn.memory, stack[I])...};
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

        fn.signature(fn.memory, fn.misc, stack + args.size());

        runtime::trap_buf = prev_buf;
        runtime::call_stack_depth = prev_depth;

        return std::vector<runtime::WasmValue>(stack, stack + fn.type.results);
    };
}

template <typename Tuple> struct remove_first_type;

template <typename First, typename... Rest>
struct remove_first_type<std::tuple<First, Rest...>> {
    using type = std::tuple<Rest...>;
};

template <> struct remove_first_type<std::tuple<>> {
    using type = std::tuple<>;
};

template <typename Tuple>
using remove_first_type_t = typename remove_first_type<Tuple>::type;

template <typename T> struct remove_first_param;

template <typename R, typename P1, typename... Args>
struct remove_first_param<R(P1, Args...)> {
    using type = R(Args...);
};

template <typename R> struct remove_first_param<R()> {
    using type = R();
};

template <typename T>
using remove_first_param_t = typename remove_first_param<T>::type;

template <class Args, size_t... I>
constexpr bool check_arg_triviality(std::index_sequence<I...>) {
    auto is_ok = [](auto x) {
        auto is_arith = std::is_arithmetic_v<decltype(x)>;
        auto is_ptr = std::is_pointer_v<decltype(x)>;
        auto is_arith_ptr =
            is_ptr && std::is_arithmetic_v<std::remove_pointer_t<decltype(x)>>;
        auto is_void_ptr =
            is_ptr && std::is_void_v<std::remove_pointer_t<decltype(x)>>;
        return is_arith || is_arith_ptr || is_void_ptr;
    };

    return (!is_ok(std::get<I>(Args{})) || ...);
}

template <auto func> constexpr bool check_arg_triviality() {
    using Fn = function_traits<decltype(func)>;
    using Args = typename Fn::args;

    return check_arg_triviality<Args>(
        std::make_index_sequence<Fn::parameter_arity>{});
}

template <auto func> runtime::TemplessSignature *wasm_functionify() {
    using BaseFn = function_traits<decltype(func)>;
    using BaseArgs = typename BaseFn::args;
    constexpr auto arity = BaseFn::result_arity;

    // check if any of the arg types are non-void pointers or non-scalars
    constexpr auto has_nontrivial_args = check_arg_triviality<BaseArgs>(
        std::make_index_sequence<std::tuple_size_v<BaseArgs>>{});
    if constexpr (has_nontrivial_args) {
        static_assert(
            std::is_same_v<std::tuple_element_t<0, BaseArgs>, std::byte *>,
            "functions with non-trivial arguments must take the wasm "
            "memory pointer as their first argument");
    }

    // skip the memory arg
    using Args = std::conditional_t<has_nontrivial_args,
                                    remove_first_type_t<BaseArgs>, BaseArgs>;

    constexpr auto n_args = std::tuple_size_v<Args>;

    return [](auto memory, auto misc, auto stack) {
        stack -= n_args;

        // Convert input arguments to tuple
        auto args = [&]<size_t... I>(std::index_sequence<I...>) {
            return Args{
                normalize<std::tuple_element_t<I, Args>>(memory, stack[I])...};
        }(std::make_index_sequence<n_args>{});

        auto run = [&] {
            if constexpr (has_nontrivial_args) {
                return std::apply(
                    func, std::tuple_cat(std::make_tuple(memory), args));
            } else {
                return std::apply(func, args);
            }
        };

        if constexpr (arity == 0) {
            run();
        } else if constexpr (arity == 1) {
            *stack = run();
        } else {
            push_tuple_to_wasm(run(), stack, std::make_index_sequence<arity>{});
        }
        stack += arity;

        return runtime::dummy(memory, misc, stack, 0, 0);
    };
}

template <auto func> runtime::FunctionInfo internalize() {
    static void *misc[] = {&runtime::WasmMemory::empty};
    auto memory = runtime::WasmMemory::empty.memory.get();

    constexpr auto has_nontrivial_args = check_arg_triviality<func>();
    using WasmType = std::conditional_t<
        has_nontrivial_args,
        remove_first_param_t<std::remove_pointer_t<decltype(func)>>,
        decltype(func)>;
    return runtime::FunctionInfo(WasmSignature::from_type<WasmType>(), nullptr,
                                 nullptr, wasm_functionify<func>(), nullptr);
}

} // namespace mitey
