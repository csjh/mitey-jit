#pragma once

#include "pager/executable.hpp"
#include "runtime.hpp"
#include "spec.hpp"
#include <span>

namespace mitey {

class Module;

template <typename Pager, typename Target> class JIT {
    std::shared_ptr<Module> mod;

    std::tuple<std::unique_ptr<uint8_t[]>, uint32_t,
               std::unique_ptr<uint32_t[]>>
    generate_prefix_array(std::span<uint8_t> bytes);

  public:
    JIT<Pager, Target>(std::shared_ptr<Module> mod) : mod(mod) {}

    std::tuple<Allocation, std::unique_ptr<uint32_t[]>>
    compile(std::span<uint8_t> bytes);
};

namespace {

template <typename Iter> static inline void skip_leb128(Iter &iter) {
    while (*iter++ & 0x80) [[unlikely]] {
    };
}

template <typename Iter> inline void skip_blocktype(Iter &iter) {
    uint8_t byte = *iter;
    if (byte == static_cast<uint8_t>(valtype::empty)) {
        ++iter;
    } else if (is_valtype(byte)) {
        ++iter;
    } else {
        skip_leb128(iter);
    }
}

} // namespace

template <typename Pager, typename Target>
std::tuple<std::unique_ptr<uint8_t[]>, uint32_t, std::unique_ptr<uint32_t[]>>
JIT<Pager, Target>::generate_prefix_array(std::span<uint8_t> bytes) {
    const size_t prelude = Target().prelude_size(),
                 postlude = Target().postlude_size(),
                 tempx0 = Target().call_size(),
                 tempx1 = tempx0 + Target().temp1_size(),
                 tempx2 = tempx1 + Target().temp2_size(), noooop = 0;

    auto iter = bytes.begin();
    auto n_functions = safe_read_leb128<uint32_t>(iter);
    auto function_offsets = std::make_unique<uint32_t[]>(n_functions);

    // get upper bound on block count
    auto blocks = std::count_if(bytes.begin(), bytes.end(), [](auto b) {
        return b == 2 || b == 3 || b == 4 || b == 5;
    });

    auto prefix = std::make_unique<uint8_t[]>(1 + n_functions + blocks);
    auto ptr = prefix.get();

    auto add = [&](uint8_t byte) { *ptr += byte; };

    using enum Instruction;
    for (uint32_t i = 0; i < n_functions; ++i) {
        auto func_length = safe_read_leb128<uint32_t>(iter);
        auto fn_end = iter + func_length;

        auto n_local_decls = safe_read_leb128<uint32_t>(iter);
        while (n_local_decls--) {
            skip_leb128(iter);
            iter++;
        }

        // clang-format off
        function_offsets[i] = *ptr;
        ptr++;
        add(prelude);
        while (iter != fn_end) {
        uint8_t byte = *iter++;
        switch (static_cast<Instruction>(byte)) {
        case br_table: {
            auto n_targets = safe_read_leb128<uint32_t>(iter);
            add(tempx2 + n_targets * sizeof(BrTableTarget));

            for (uint32_t i = 0; i <= n_targets; ++i) {
                skip_leb128(iter);
            }
            break;
        }
        case unreachable:                                       add(tempx0); break;
        case nop:                                               add(noooop); break;
        case block:    skip_blocktype(iter);                    ptr++; add(noooop); break;
        case loop:     skip_blocktype(iter);                    ptr++; add(noooop); break;
        case if_:      skip_blocktype(iter);                    ptr++; add(tempx1); break;
        case else_:                                             ptr++; add(tempx1); break;
        case end:                                               add(noooop); break;
        case br:                                                add(tempx2); break;
        case br_if:                                             add(tempx2); break;
        case return_:                                           add(tempx2); break;
        case call:        skip_leb128(iter);                    add(tempx1); break;
        case call_indirect:                                       /* todo */ break;
        case drop:                                              add(tempx0); break;
        case select:                                            add(tempx0); break;
        case select_t:                                            /* todo */ break;
        case localget:    skip_leb128(iter);                    add(tempx1); break;
        case localset:    skip_leb128(iter);                    add(tempx1); break;
        case localtee:    skip_leb128(iter);                    add(tempx1); break;
        case tableget:    skip_leb128(iter);                    add(tempx1); break;
        case tableset:    skip_leb128(iter);                    add(tempx1); break;
        case globalget:   skip_leb128(iter);                    add(tempx1); break;
        case globalset:   skip_leb128(iter);                    add(tempx1); break;
        case memorysize:             iter++;                    add(tempx0); break;
        case memorygrow:             iter++;                    add(tempx0); break;
        case i32const:    skip_leb128(iter);                    add(tempx1); break;
        case i64const:    skip_leb128(iter);                    add(tempx1); break;
        case f32const:    skip_leb128(iter);                    add(tempx1); break;
        case f64const:    skip_leb128(iter);                    add(tempx1); break;
        case i32load:     skip_leb128(iter); skip_leb128(iter); add(tempx1); break;
        case i64load:     skip_leb128(iter); skip_leb128(iter); add(tempx1); break;
        case f32load:     skip_leb128(iter); skip_leb128(iter); add(tempx1); break;
        case f64load:     skip_leb128(iter); skip_leb128(iter); add(tempx1); break;
        case i32load8_s:  skip_leb128(iter); skip_leb128(iter); add(tempx1); break;
        case i32load8_u:  skip_leb128(iter); skip_leb128(iter); add(tempx1); break;
        case i32load16_s: skip_leb128(iter); skip_leb128(iter); add(tempx1); break;
        case i32load16_u: skip_leb128(iter); skip_leb128(iter); add(tempx1); break;
        case i64load8_s:  skip_leb128(iter); skip_leb128(iter); add(tempx1); break;
        case i64load8_u:  skip_leb128(iter); skip_leb128(iter); add(tempx1); break;
        case i64load16_s: skip_leb128(iter); skip_leb128(iter); add(tempx1); break;
        case i64load16_u: skip_leb128(iter); skip_leb128(iter); add(tempx1); break;
        case i64load32_s: skip_leb128(iter); skip_leb128(iter); add(tempx1); break;
        case i64load32_u: skip_leb128(iter); skip_leb128(iter); add(tempx1); break;
        case i32store:    skip_leb128(iter); skip_leb128(iter); add(tempx1); break;
        case i64store:    skip_leb128(iter); skip_leb128(iter); add(tempx1); break;
        case f32store:    skip_leb128(iter); skip_leb128(iter); add(tempx1); break;
        case f64store:    skip_leb128(iter); skip_leb128(iter); add(tempx1); break;
        case i32store8:   skip_leb128(iter); skip_leb128(iter); add(tempx1); break;
        case i32store16:  skip_leb128(iter); skip_leb128(iter); add(tempx1); break;
        case i64store8:   skip_leb128(iter); skip_leb128(iter); add(tempx1); break;
        case i64store16:  skip_leb128(iter); skip_leb128(iter); add(tempx1); break;
        case i64store32:  skip_leb128(iter); skip_leb128(iter); add(tempx1); break;
        case i32eqz:                                            add(tempx0); break;
        case i64eqz:                                            add(tempx0); break;
        case i32eq:                                             add(tempx0); break;
        case i64eq:                                             add(tempx0); break;
        case i32ne:                                             add(tempx0); break;
        case i64ne:                                             add(tempx0); break;
        case i32lt_s:                                           add(tempx0); break;
        case i64lt_s:                                           add(tempx0); break;
        case i32lt_u:                                           add(tempx0); break;
        case i64lt_u:                                           add(tempx0); break;
        case i32gt_s:                                           add(tempx0); break;
        case i64gt_s:                                           add(tempx0); break;
        case i32gt_u:                                           add(tempx0); break;
        case i64gt_u:                                           add(tempx0); break;
        case i32le_s:                                           add(tempx0); break;
        case i64le_s:                                           add(tempx0); break;
        case i32le_u:                                           add(tempx0); break;
        case i64le_u:                                           add(tempx0); break;
        case i32ge_s:                                           add(tempx0); break;
        case i64ge_s:                                           add(tempx0); break;
        case i32ge_u:                                           add(tempx0); break;
        case i64ge_u:                                           add(tempx0); break;
        case f32eq:                                             add(tempx0); break;
        case f64eq:                                             add(tempx0); break;
        case f32ne:                                             add(tempx0); break;
        case f64ne:                                             add(tempx0); break;
        case f32lt:                                             add(tempx0); break;
        case f64lt:                                             add(tempx0); break;
        case f32gt:                                             add(tempx0); break;
        case f64gt:                                             add(tempx0); break;
        case f32le:                                             add(tempx0); break;
        case f64le:                                             add(tempx0); break;
        case f32ge:                                             add(tempx0); break;
        case f64ge:                                             add(tempx0); break;
        case i32clz:                                            add(tempx0); break;
        case i64clz:                                            add(tempx0); break;
        case i32ctz:                                            add(tempx0); break;
        case i64ctz:                                            add(tempx0); break;
        case i32popcnt:                                         add(tempx0); break;
        case i64popcnt:                                         add(tempx0); break;
        case i32add:                                            add(tempx0); break;
        case i64add:                                            add(tempx0); break;
        case i32sub:                                            add(tempx0); break;
        case i64sub:                                            add(tempx0); break;
        case i32mul:                                            add(tempx0); break;
        case i64mul:                                            add(tempx0); break;
        case i32div_s:                                          add(tempx0); break;
        case i64div_s:                                          add(tempx0); break;
        case i32div_u:                                          add(tempx0); break;
        case i64div_u:                                          add(tempx0); break;
        case i32rem_s:                                          add(tempx0); break;
        case i64rem_s:                                          add(tempx0); break;
        case i32rem_u:                                          add(tempx0); break;
        case i64rem_u:                                          add(tempx0); break;
        case i32and:                                            add(tempx0); break;
        case i64and:                                            add(tempx0); break;
        case i32or:                                             add(tempx0); break;
        case i64or:                                             add(tempx0); break;
        case i32xor:                                            add(tempx0); break;
        case i64xor:                                            add(tempx0); break;
        case i32shl:                                            add(tempx0); break;
        case i64shl:                                            add(tempx0); break;
        case i32shr_s:                                          add(tempx0); break;
        case i64shr_s:                                          add(tempx0); break;
        case i32shr_u:                                          add(tempx0); break;
        case i64shr_u:                                          add(tempx0); break;
        case i32rotl:                                           add(tempx0); break;
        case i64rotl:                                           add(tempx0); break;
        case i32rotr:                                           add(tempx0); break;
        case i64rotr:                                           add(tempx0); break;
        case f32abs:                                            add(tempx0); break;
        case f64abs:                                            add(tempx0); break;
        case f32neg:                                            add(tempx0); break;
        case f64neg:                                            add(tempx0); break;
        case f32ceil:                                           add(tempx0); break;
        case f64ceil:                                           add(tempx0); break;
        case f32floor:                                          add(tempx0); break;
        case f64floor:                                          add(tempx0); break;
        case f32trunc:                                          add(tempx0); break;
        case f64trunc:                                          add(tempx0); break;
        case f32nearest:                                        add(tempx0); break;
        case f64nearest:                                        add(tempx0); break;
        case f32sqrt:                                           add(tempx0); break;
        case f64sqrt:                                           add(tempx0); break;
        case f32add:                                            add(tempx0); break;
        case f64add:                                            add(tempx0); break;
        case f32sub:                                            add(tempx0); break;
        case f64sub:                                            add(tempx0); break;
        case f32mul:                                            add(tempx0); break;
        case f64mul:                                            add(tempx0); break;
        case f32div:                                            add(tempx0); break;
        case f64div:                                            add(tempx0); break;
        case f32min:                                            add(tempx0); break;
        case f64min:                                            add(tempx0); break;
        case f32max:                                            add(tempx0); break;
        case f64max:                                            add(tempx0); break;
        case f32copysign:                                       add(tempx0); break;
        case f64copysign:                                       add(tempx0); break;
        case i32wrap_i64:                                       add(tempx0); break;
        case i64extend_i32_s:                                   add(tempx0); break;
        case i64extend_i32_u:                                   add(tempx0); break;
        case i32trunc_f32_s:                                    add(tempx0); break;
        case i64trunc_f32_s:                                    add(tempx0); break;
        case i32trunc_f32_u:                                    add(tempx0); break;
        case i64trunc_f32_u:                                    add(tempx0); break;
        case i32trunc_f64_s:                                    add(tempx0); break;
        case i64trunc_f64_s:                                    add(tempx0); break;
        case i32trunc_f64_u:                                    add(tempx0); break;
        case i64trunc_f64_u:                                    add(tempx0); break;
        case f32convert_i32_s:                                  add(tempx0); break;
        case f64convert_i32_s:                                  add(tempx0); break;
        case f32convert_i32_u:                                  add(tempx0); break;
        case f64convert_i32_u:                                  add(tempx0); break;
        case f32convert_i64_s:                                  add(tempx0); break;
        case f64convert_i64_s:                                  add(tempx0); break;
        case f32convert_i64_u:                                  add(tempx0); break;
        case f64convert_i64_u:                                  add(tempx0); break;
        case f32demote_f64:                                     add(tempx0); break;
        case f64promote_f32:                                    add(tempx0); break;
        case i32reinterpret_f32:                                add(tempx0); break;
        case f32reinterpret_i32:                                add(tempx0); break;
        case i64reinterpret_f64:                                add(tempx0); break;
        case f64reinterpret_i64:                                add(tempx0); break;
        case i32extend8_s:                                      add(tempx0); break;
        case i32extend16_s:                                     add(tempx0); break;
        case i64extend8_s:                                      add(tempx0); break;
        case i64extend16_s:                                     add(tempx0); break;
        case i64extend32_s:                                     add(tempx0); break;
        case ref_null:    skip_leb128(iter);                    add(tempx0); break;
        case ref_is_null:                                       add(tempx0); break;
        case ref_func:    skip_leb128(iter);                    add(tempx1); break;
        case ref_eq:                                            add(tempx0); break;
        case multibyte: {
            auto byte = safe_read_leb128<uint32_t>(iter);

            using enum FCInstruction;
            switch (static_cast<FCInstruction>(byte)) {
                case i32_trunc_sat_f32_s:                               add(tempx0); break;
                case i32_trunc_sat_f32_u:                               add(tempx0); break;
                case i32_trunc_sat_f64_s:                               add(tempx0); break;
                case i32_trunc_sat_f64_u:                               add(tempx0); break;
                case i64_trunc_sat_f32_s:                               add(tempx0); break;
                case i64_trunc_sat_f32_u:                               add(tempx0); break;
                case i64_trunc_sat_f64_s:                               add(tempx0); break;
                case i64_trunc_sat_f64_u:                               add(tempx0); break;
                case memory_init: skip_leb128(iter);         iter += 1; add(tempx1); break;
                case data_drop:   skip_leb128(iter);                    add(tempx1); break;
                case memory_copy:                            iter += 2; add(tempx0); break;
                case memory_fill:                            iter += 1; add(tempx0); break;
                case table_init:  skip_leb128(iter); skip_leb128(iter); add(tempx2); break;
                case elem_drop:   skip_leb128(iter);                    add(tempx1); break;
                case table_copy:  skip_leb128(iter); skip_leb128(iter); add(tempx2); break;
                case table_grow:  skip_leb128(iter);                    add(tempx1); break;
                case table_size:  skip_leb128(iter);                    add(tempx1); break;
                case table_fill:  skip_leb128(iter);                    add(tempx1); break;
                default: __builtin_unreachable();
            }
            break;
        }
        default: __builtin_unreachable();
        };
        }
        add(postlude);
        // clang-format on
    }
    ptr++;

    return {std::move(prefix), ptr - prefix.get(), std::move(function_offsets)};
}

template <typename Pager, typename Target>
std::tuple<Allocation, std::unique_ptr<uint32_t[]>>
JIT<Pager, Target>::compile(std::span<uint8_t> bytes) {

    auto [prefix, length, function_offsets] = generate_prefix_array(bytes);
    auto alloc = Pager().allocate(prefix[length - 1]);

    Pager().write(alloc, [&] {});

    return {std::move(alloc), std::move(function_offsets)};
}

} // namespace mitey