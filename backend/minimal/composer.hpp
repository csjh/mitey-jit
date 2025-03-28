#include "../../module.hpp"
#include "../../runtime.hpp"
#include <cstdint>
#include <cstring>

namespace mitey {

#define SHARED_PARAMS                                                          \
    [[maybe_unused]] std::byte *&code, [[maybe_unused]] WasmStack &stack

template <typename Target> class Composer {
#define nilary(name)                                                           \
    void name(SHARED_PARAMS) { Target::put_call(code, runtime::name); }

#define unary(name)                                                            \
    void name(SHARED_PARAMS, uint64_t tmp1) {                                  \
        Target::put_temp1(code, tmp1);                                         \
        Target::put_call(code, runtime::name);                                 \
    }

#define binary(name)                                                           \
    void name(SHARED_PARAMS, uint64_t tmp1, uint64_t tmp2) {                   \
        Target::put_temp1(code, tmp1);                                         \
        Target::put_temp2(code, tmp2);                                         \
        Target::put_call(code, runtime::name);                                 \
    }

#define memop(name)                                                            \
    void name(SHARED_PARAMS, uint64_t offset,                                  \
              [[maybe_unused]] uint64_t align) {                               \
        Target::put_temp1(code, offset);                                       \
        Target::put_call(code, runtime::name);                                 \
    }

    void exit_function(SHARED_PARAMS, FunctionShell &fn) {
        // move results past locals
        Target::put_temp1(code, -fn.locals.bytesize());
        auto byte_results = fn.type.results.bytesize();
        if (byte_results == 0) {
            Target::put_call(code, runtime::move_0_results);
        } else if (byte_results == 8) {
            Target::put_call(code, runtime::move_8_results);
        } else {
            Target::put_temp2(code, -byte_results);
            Target::put_call(code, runtime::move_n_results);
        }

        Target::put_postlude(code);
    }

  public:
    static constexpr size_t function_overhead =
        Target::max_prelude_size + Target::max_postlude_size;
    static constexpr size_t max_instruction =
        Target::max_call_size + Target::max_temp1_size + Target::max_temp2_size;

    void start_function(SHARED_PARAMS, FunctionShell &fn) {
        auto locals_bytes = fn.locals.bytesize() - fn.type.params.bytesize();

        Target::put_prelude(code);
        Target::put_temp1(code, fn.type.params.bytesize());
        Target::put_temp2(code, locals_bytes);
        Target::put_call(code, runtime::clear_locals);
    }

    nilary(unreachable);
    void nop(SHARED_PARAMS) {}
    void block(SHARED_PARAMS, WasmSignature &) {}
    void loop(SHARED_PARAMS, WasmSignature &) {}
    std::byte *if_(SHARED_PARAMS, WasmSignature &) {
        return Target::put_if(code);
    }
    void else_(SHARED_PARAMS, std::span<ControlFlow> control_stack) {
        auto &if_flow = control_stack.back();

        auto imm = Target::put_br(code, 0, 0);
        if_flow.pending_br.push_back(imm);

        Target::put_immediate(std::get<If>(if_flow.construct).else_jump, code);
    }
    void end(SHARED_PARAMS, ControlFlow &flow) {
        if (std::holds_alternative<If>(flow.construct)) {
            Target::put_immediate(std::get<If>(flow.construct).else_jump, code);
        }

        if (!std::holds_alternative<Loop>(flow.construct)) {
            for (auto target : flow.pending_br) {
                Target::put_immediate(target, code);
            }
            for (auto [table, target] : flow.pending_br_tables) {
                auto diff = code - table;
                auto idiff = static_cast<int32_t>(diff);
                ensure(idiff == diff, "branch target out of range");
                std::memcpy(target, &idiff, sizeof(idiff));
            }
        }

        if (std::holds_alternative<Function>(flow.construct)) {
            exit_function(code, stack, std::get<Function>(flow.construct).fn);
        }
    }
    void br(SHARED_PARAMS, std::span<ControlFlow> control_stack,
            uint32_t depth) {
        auto &flow = control_stack[control_stack.size() - depth - 1];

        auto imm = Target::put_br(code, flow.expected.bytesize(),
                                  flow.stack_offset - stack.sp());

        if (std::holds_alternative<Loop>(flow.construct)) {
            Target::put_immediate(imm, std::get<Loop>(flow.construct).start);
        } else {
            flow.pending_br.push_back(imm);
        }
    }
    void br_if(SHARED_PARAMS, std::span<ControlFlow> control_stack,
               uint32_t depth) {
        auto &flow = control_stack[control_stack.size() - depth - 1];

        auto imm = Target::put_br_if(code, flow.expected.bytesize(),
                                     flow.stack_offset - stack.sp());

        if (std::holds_alternative<Loop>(flow.construct)) {
            Target::put_immediate(imm, std::get<Loop>(flow.construct).start);
        } else {
            flow.pending_br.push_back(imm);
        }
    }
    void br_table(SHARED_PARAMS, std::span<ControlFlow> control_stack,
                  std::span<uint32_t> targets) {
        auto t1_addr = code;
        Target::placehold(code, Target::put_temp1);
        auto t2_addr = code;
        Target::placehold(code, Target::put_temp2);
        auto call_addr = code;
        Target::placehold(code, Target::put_call);
        auto table_addr = code;

        Target::put_temp1(t1_addr, reinterpret_cast<uint64_t>(table_addr));

        auto base = control_stack.size() - 1;
        auto &default_target = control_stack[base - targets.back()].expected;

        auto info =
            runtime::BrInfo(targets.size() - 1, default_target.bytesize());
        if (info.arity == 0) {
            Target::put_call(call_addr, runtime::br_table_0);
        } else if (info.arity == 8) {
            Target::put_call(call_addr, runtime::br_table_8);
        } else {
            Target::put_call(call_addr, runtime::br_table_n);
        }
        Target::put_temp2(t2_addr, std::bit_cast<uint64_t>(info));

        for (auto depth : targets) {
            auto &flow = control_stack[base - depth];
            auto offset = static_cast<int32_t>(flow.stack_offset - stack.sp());
            if (std::holds_alternative<Loop>(flow.construct)) {
                auto target = runtime::BrTableTarget(
                    std::get<Loop>(flow.construct).start - table_addr, offset);
                std::memcpy(code, &target, sizeof(target));
                code += sizeof(target);
            } else {
                flow.pending_br_tables.push_back(
                    PendingBrTable(table_addr, code));
                code += sizeof(uint32_t);
                std::memcpy(code, &offset, sizeof(offset));
                code += sizeof(offset);
            }
        }
    }
    void return_(SHARED_PARAMS, std::span<ControlFlow> control_stack) {
        br(code, stack, control_stack, control_stack.size() - 1);
    }
    void call(SHARED_PARAMS, FunctionShell &fn, uint32_t func_offset) {
        Target::put_temp1(code, func_offset);
        Target::put_temp2(code, (fn.type.params.size() << 32) |
                                    fn.type.results.size());
        Target::put_call(code, runtime::call_extern);
    }
    void call_indirect(SHARED_PARAMS, uint32_t table_offset,
                       WasmSignature &type) {
        auto info = runtime::CallIndirectInfo(table_offset,
                                              runtime::FunctionType(type));
        auto [temp1, temp2] = std::bit_cast<std::array<uint64_t, 2>>(info);

        Target::put_temp1(code, temp1);
        Target::put_temp2(code, temp2);
        Target::put_call(code, runtime::call_indirect);
    }
    void drop(SHARED_PARAMS, valtype) { Target::put_call(code, runtime::drop); }
    void select(SHARED_PARAMS, valtype) {
        Target::put_call(code, runtime::select);
    }
    void select_t(SHARED_PARAMS, valtype) {
        Target::put_call(code, runtime::select_t);
    }
    void localget(SHARED_PARAMS, FunctionShell &fn, uint32_t local_idx) {
        Target::put_temp1(code, -(stack.sp() + fn.local_bytes[local_idx] -
                                  valtype_size(fn.locals[local_idx])));
        Target::put_call(code, runtime::localget);
    }
    void localset(SHARED_PARAMS, FunctionShell &fn, uint32_t local_idx) {
        Target::put_temp1(code, -(stack.sp() + fn.local_bytes[local_idx] +
                                  valtype_size(fn.locals[local_idx])));
        Target::put_call(code, runtime::localset);
    }
    void localtee(SHARED_PARAMS, FunctionShell &fn, uint32_t local_idx) {
        Target::put_temp1(code, -(stack.sp() + fn.local_bytes[local_idx]));
        Target::put_call(code, runtime::localtee);
    }
    unary(tableget);
    unary(tableset);
    void globalget(std::byte *&code, WasmStack &stack, uint64_t tmp1, valtype) {
        Target::put_temp1(code, tmp1);
        Target::put_call(code, runtime::globalget);
    }
    void globalset(std::byte *&code, WasmStack &stack, uint64_t tmp1, valtype) {
        Target::put_temp1(code, tmp1);
        Target::put_call(code, runtime::globalset);
    }
    nilary(memorysize);
    nilary(memorygrow);
    void i32const(SHARED_PARAMS, uint32_t cons) {
        runtime::WasmValue v;
        std::memcpy(&v.i32, &cons, sizeof(cons));
        Target::put_temp1(code, v.u64);
        Target::put_call(code, runtime::ifXXconst);
    }
    void i64const(SHARED_PARAMS, uint64_t cons) {
        runtime::WasmValue v;
        std::memcpy(&v.i64, &cons, sizeof(cons));
        Target::put_temp1(code, v.u64);
        Target::put_call(code, runtime::ifXXconst);
    }
    void f32const(SHARED_PARAMS, float cons) {
        runtime::WasmValue v;
        std::memcpy(&v.f32, &cons, sizeof(cons));
        Target::put_temp1(code, v.u64);
        Target::put_call(code, runtime::ifXXconst);
    }
    void f64const(SHARED_PARAMS, double cons) {
        runtime::WasmValue v;
        std::memcpy(&v.f64, &cons, sizeof(cons));
        Target::put_temp1(code, v.u64);
        Target::put_call(code, runtime::ifXXconst);
    }
    memop(i32load);
    memop(i64load);
    memop(f32load);
    memop(f64load);
    memop(i32load8_s);
    memop(i32load8_u);
    memop(i32load16_s);
    memop(i32load16_u);
    memop(i64load8_s);
    memop(i64load8_u);
    memop(i64load16_s);
    memop(i64load16_u);
    memop(i64load32_s);
    memop(i64load32_u);
    memop(i32store);
    memop(i64store);
    memop(f32store);
    memop(f64store);
    memop(i32store8);
    memop(i32store16);
    memop(i64store8);
    memop(i64store16);
    memop(i64store32);
    nilary(i32eqz);
    nilary(i64eqz);
    nilary(i32eq);
    nilary(i64eq);
    nilary(i32ne);
    nilary(i64ne);
    nilary(i32lt_s);
    nilary(i64lt_s);
    nilary(i32lt_u);
    nilary(i64lt_u);
    nilary(i32gt_s);
    nilary(i64gt_s);
    nilary(i32gt_u);
    nilary(i64gt_u);
    nilary(i32le_s);
    nilary(i64le_s);
    nilary(i32le_u);
    nilary(i64le_u);
    nilary(i32ge_s);
    nilary(i64ge_s);
    nilary(i32ge_u);
    nilary(i64ge_u);
    nilary(f32eq);
    nilary(f64eq);
    nilary(f32ne);
    nilary(f64ne);
    nilary(f32lt);
    nilary(f64lt);
    nilary(f32gt);
    nilary(f64gt);
    nilary(f32le);
    nilary(f64le);
    nilary(f32ge);
    nilary(f64ge);
    nilary(i32clz);
    nilary(i64clz);
    nilary(i32ctz);
    nilary(i64ctz);
    nilary(i32popcnt);
    nilary(i64popcnt);
    nilary(i32add);
    nilary(i64add);
    nilary(i32sub);
    nilary(i64sub);
    nilary(i32mul);
    nilary(i64mul);
    nilary(i32div_s);
    nilary(i64div_s);
    nilary(i32div_u);
    nilary(i64div_u);
    nilary(i32rem_s);
    nilary(i64rem_s);
    nilary(i32rem_u);
    nilary(i64rem_u);
    nilary(i32and);
    nilary(i64and);
    nilary(i32or);
    nilary(i64or);
    nilary(i32xor);
    nilary(i64xor);
    nilary(i32shl);
    nilary(i64shl);
    nilary(i32shr_s);
    nilary(i64shr_s);
    nilary(i32shr_u);
    nilary(i64shr_u);
    nilary(i32rotl);
    nilary(i64rotl);
    nilary(i32rotr);
    nilary(i64rotr);
    nilary(f32abs);
    nilary(f64abs);
    nilary(f32neg);
    nilary(f64neg);
    nilary(f32ceil);
    nilary(f64ceil);
    nilary(f32floor);
    nilary(f64floor);
    nilary(f32trunc);
    nilary(f64trunc);
    nilary(f32nearest);
    nilary(f64nearest);
    nilary(f32sqrt);
    nilary(f64sqrt);
    nilary(f32add);
    nilary(f64add);
    nilary(f32sub);
    nilary(f64sub);
    nilary(f32mul);
    nilary(f64mul);
    nilary(f32div);
    nilary(f64div);
    nilary(f32min);
    nilary(f64min);
    nilary(f32max);
    nilary(f64max);
    nilary(f32copysign);
    nilary(f64copysign);
    nilary(i32wrap_i64);
    nilary(i64extend_i32_s);
    nilary(i64extend_i32_u);
    nilary(i32trunc_f32_s);
    nilary(i64trunc_f32_s);
    nilary(i32trunc_f32_u);
    nilary(i64trunc_f32_u);
    nilary(i32trunc_f64_s);
    nilary(i64trunc_f64_s);
    nilary(i32trunc_f64_u);
    nilary(i64trunc_f64_u);
    nilary(f32convert_i32_s);
    nilary(f64convert_i32_s);
    nilary(f32convert_i32_u);
    nilary(f64convert_i32_u);
    nilary(f32convert_i64_s);
    nilary(f64convert_i64_s);
    nilary(f32convert_i64_u);
    nilary(f64convert_i64_u);
    nilary(f32demote_f64);
    nilary(f64promote_f32);
    void i32reinterpret_f32(SHARED_PARAMS) {}
    void f32reinterpret_i32(SHARED_PARAMS) {}
    void i64reinterpret_f64(SHARED_PARAMS) {}
    void f64reinterpret_i64(SHARED_PARAMS) {}
    nilary(i32extend8_s);
    nilary(i32extend16_s);
    nilary(i64extend8_s);
    nilary(i64extend16_s);
    nilary(i64extend32_s);
    nilary(ref_null);
    nilary(ref_is_null);
    unary(ref_func);
    nilary(ref_eq);
    nilary(i32_trunc_sat_f32_s);
    nilary(i32_trunc_sat_f32_u);
    nilary(i32_trunc_sat_f64_s);
    nilary(i32_trunc_sat_f64_u);
    nilary(i64_trunc_sat_f32_s);
    nilary(i64_trunc_sat_f32_u);
    nilary(i64_trunc_sat_f64_s);
    nilary(i64_trunc_sat_f64_u);
    unary(memory_init);
    unary(data_drop);
    nilary(memory_copy);
    nilary(memory_fill);
    binary(table_init);
    unary(elem_drop);
    binary(table_copy);
    unary(table_grow);
    unary(table_size);
    unary(table_fill);
};

#undef nilary
#undef unary
#undef binary
#undef memop

} // namespace mitey