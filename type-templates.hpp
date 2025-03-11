#pragma once

#include <tuple>

namespace mitey {

template <template <typename...> class T, typename U>
constexpr bool is_specialization_of = false;

template <template <typename...> class T, typename... Us>
constexpr bool is_specialization_of<T, T<Us...>> = true;

template <template <auto...> class T, typename U>
constexpr bool is_value_specialization_of = false;

template <template <auto...> class T, auto... Us>
constexpr bool is_value_specialization_of<T, T<Us...>> = true;

template <typename T>
using TupleIfNotTuple = std::conditional_t<
    std::is_same_v<T, void>, std::tuple<>,
    std::conditional_t<is_specialization_of<std::tuple, T>, T, std::tuple<T>>>;

template <typename T> struct function_traits;

template <typename R, typename... Args> struct function_traits<R (*)(Args...)> {
    using args = std::tuple<Args...>;
    using return_type = R;
    using return_type_tuple = TupleIfNotTuple<R>;

    static constexpr size_t parameter_arity = std::tuple_size_v<args>;
    static constexpr size_t result_arity = std::tuple_size_v<return_type_tuple>;
};

template <typename R, typename... Args>
struct function_traits<R(Args...)> : function_traits<R (*)(Args...)> {};

} // namespace mitey