#include "../../module.hpp"
#include "../../runtime.hpp"
#include <cstdint>
#include <cstring>

namespace mitey {

#define SHARED_PARAMS std::byte *&code, WasmStack &stack, extra &_extra

template <typename Target> class Composer {
  public:
    // unused
    using extra = int;

  private:
    template <runtime::Signature func> static void nilary(SHARED_PARAMS) {
        Target::put_call(code, func);
    }

    template <runtime::Signature func>
    static void unary(SHARED_PARAMS, uint64_t tmp1) {
        Target::put_temp1(code, tmp1);
        Target::put_call(code, func);
    }

    template <runtime::Signature func>
    static void binary(SHARED_PARAMS, uint64_t tmp1, uint64_t tmp2) {
        Target::put_temp1(code, tmp1);
        Target::put_temp2(code, tmp2);
        Target::put_call(code, func);
    }

    template <runtime::Signature func>
    static void memop(SHARED_PARAMS, uint64_t offset, uint64_t align) {
        Target::put_temp1(code, offset);
        Target::put_call(code, func);
    }

  public:
    static constexpr size_t function_overhead =
        Target::max_prelude_size + Target::max_postlude_size;
    static constexpr size_t max_instruction =
        Target::max_call_size + Target::max_temp1_size + Target::max_temp2_size;

    static void put_call_address(std::byte *&code, std::byte *func) {
        Target::put_temp1(code, reinterpret_cast<uint64_t>(func));
    }

    static void start_function(SHARED_PARAMS, FunctionShell &fn) {
        auto locals_bytes = fn.locals.bytesize() - fn.type.params.bytesize();

        Target::put_prelude(code);
        Target::put_temp1(code, locals_bytes);
        Target::put_call(code, runtime::clear_locals);
    }
    static void exit_function(SHARED_PARAMS, FunctionShell &fn) {
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

    static constexpr auto unreachable = nilary<runtime::unreachable>;
    static void nop(SHARED_PARAMS) {}
    static void block(SHARED_PARAMS, WasmSignature &sig) {}
    static void loop(SHARED_PARAMS, WasmSignature &sig) {}
    static std::byte *if_(SHARED_PARAMS, WasmSignature &sig) {
        return Target::put_if(code);
    }
    static std::byte *else_(SHARED_PARAMS, WasmSignature &sig,
                            std::byte *if_location) {
        auto imm = Target::put_br(code, 0, 0);
        Target::put_immediate(if_location, code);
        return imm;
    }
    static void end(SHARED_PARAMS, ControlFlow &flow) {
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
            auto &fn = std::get<Function>(flow.construct).fn;

            // move results past locals
            Target::put_temp1(code, -fn.locals.bytesize());
            auto byte_results = flow.sig.results.bytesize();
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
    }
    static void br(SHARED_PARAMS, std::span<ControlFlow> control_stack,
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
    static void br_if(SHARED_PARAMS, std::span<ControlFlow> control_stack,
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
    static void br_table(SHARED_PARAMS, std::span<ControlFlow> control_stack,
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
            auto &target = control_stack[base - depth].expected;

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
    static void return_(SHARED_PARAMS, std::span<ControlFlow> control_stack) {
        br(code, stack, _extra, control_stack, control_stack.size() - 1);
    }
    static void call_extern(SHARED_PARAMS, FunctionShell &fn,
                            uint32_t func_offset) {
        Target::put_temp1(code, func_offset);
        Target::put_temp2(code, fn.type.results.bytesize() -
                                    fn.type.params.bytesize());
        Target::put_call(code, runtime::call_extern);
    }
    static std::byte *call(SHARED_PARAMS, FunctionShell &fn) {
        auto imm = code;
        Target::placehold(code, Target::put_temp1);
        Target::put_temp2(code, fn.type.results.bytesize() -
                                    fn.type.params.bytesize());
        Target::put_call(code, runtime::call);
        return imm;
    }
    static void call_indirect(SHARED_PARAMS, uint32_t table_offset,
                              WasmSignature &type) {
        auto info = runtime::CallIndirectInfo(table_offset,
                                              runtime::FunctionType(type));
        auto [temp1, temp2] = std::bit_cast<std::array<uint64_t, 2>>(info);

        Target::put_temp1(code, temp1);
        Target::put_temp2(code, temp2);
        Target::put_call(code, runtime::call_indirect);
    }
    static void drop(SHARED_PARAMS) { Target::put_call(code, runtime::drop); }
    static void select(SHARED_PARAMS) {
        Target::put_call(code, runtime::select);
    }
    static void select_t(SHARED_PARAMS) {
        Target::put_call(code, runtime::select_t);
    }
    static void localget(SHARED_PARAMS, FunctionShell &fn, uint32_t local_idx) {
        Target::put_temp1(code, -(stack.sp() + fn.local_bytes[local_idx] -
                                  valtype_size(fn.locals[local_idx])));
        Target::put_call(code, runtime::localget);
    }
    static void localset(SHARED_PARAMS, FunctionShell &fn, uint32_t local_idx) {
        Target::put_temp1(code, -(stack.sp() + fn.local_bytes[local_idx] +
                                  valtype_size(fn.locals[local_idx])));
        Target::put_call(code, runtime::localset);
    }
    static void localtee(SHARED_PARAMS, FunctionShell &fn, uint32_t local_idx) {
        Target::put_temp1(code, -(stack.sp() + fn.local_bytes[local_idx]));
        Target::put_call(code, runtime::localtee);
    }
    static constexpr auto tableget = unary<runtime::tableget>;
    static constexpr auto tableset = unary<runtime::tableset>;
    static constexpr auto globalget = unary<runtime::globalget>;
    static constexpr auto globalset = unary<runtime::globalset>;
    static constexpr auto memorysize = nilary<runtime::memorysize>;
    static constexpr auto memorygrow = nilary<runtime::memorygrow>;
    static void i32const(SHARED_PARAMS, uint32_t cons) {
        runtime::WasmValue v;
        std::memcpy(&v.i32, &cons, sizeof(cons));
        Target::put_temp1(code, v.u64);
        Target::put_call(code, runtime::ifXXconst);
    }
    static void i64const(SHARED_PARAMS, uint64_t cons) {
        runtime::WasmValue v;
        std::memcpy(&v.i64, &cons, sizeof(cons));
        Target::put_temp1(code, v.u64);
        Target::put_call(code, runtime::ifXXconst);
    }
    static void f32const(SHARED_PARAMS, float cons) {
        runtime::WasmValue v;
        std::memcpy(&v.f32, &cons, sizeof(cons));
        Target::put_temp1(code, v.u64);
        Target::put_call(code, runtime::ifXXconst);
    }
    static void f64const(SHARED_PARAMS, double cons) {
        runtime::WasmValue v;
        std::memcpy(&v.f64, &cons, sizeof(cons));
        Target::put_temp1(code, v.u64);
        Target::put_call(code, runtime::ifXXconst);
    }
    static constexpr auto i32load = memop<runtime::i32load>;
    static constexpr auto i64load = memop<runtime::i64load>;
    static constexpr auto f32load = memop<runtime::f32load>;
    static constexpr auto f64load = memop<runtime::f64load>;
    static constexpr auto i32load8_s = memop<runtime::i32load8_s>;
    static constexpr auto i32load8_u = memop<runtime::i32load8_u>;
    static constexpr auto i32load16_s = memop<runtime::i32load16_s>;
    static constexpr auto i32load16_u = memop<runtime::i32load16_u>;
    static constexpr auto i64load8_s = memop<runtime::i64load8_s>;
    static constexpr auto i64load8_u = memop<runtime::i64load8_u>;
    static constexpr auto i64load16_s = memop<runtime::i64load16_s>;
    static constexpr auto i64load16_u = memop<runtime::i64load16_u>;
    static constexpr auto i64load32_s = memop<runtime::i64load32_s>;
    static constexpr auto i64load32_u = memop<runtime::i64load32_u>;
    static constexpr auto i32store = memop<runtime::i32store>;
    static constexpr auto i64store = memop<runtime::i64store>;
    static constexpr auto f32store = memop<runtime::f32store>;
    static constexpr auto f64store = memop<runtime::f64store>;
    static constexpr auto i32store8 = memop<runtime::i32store8>;
    static constexpr auto i32store16 = memop<runtime::i32store16>;
    static constexpr auto i64store8 = memop<runtime::i64store8>;
    static constexpr auto i64store16 = memop<runtime::i64store16>;
    static constexpr auto i64store32 = memop<runtime::i64store32>;
    static constexpr auto i32eqz = nilary<runtime::i32eqz>;
    static constexpr auto i64eqz = nilary<runtime::i64eqz>;
    static constexpr auto i32eq = nilary<runtime::i32eq>;
    static constexpr auto i64eq = nilary<runtime::i64eq>;
    static constexpr auto i32ne = nilary<runtime::i32ne>;
    static constexpr auto i64ne = nilary<runtime::i64ne>;
    static constexpr auto i32lt_s = nilary<runtime::i32lt_s>;
    static constexpr auto i64lt_s = nilary<runtime::i64lt_s>;
    static constexpr auto i32lt_u = nilary<runtime::i32lt_u>;
    static constexpr auto i64lt_u = nilary<runtime::i64lt_u>;
    static constexpr auto i32gt_s = nilary<runtime::i32gt_s>;
    static constexpr auto i64gt_s = nilary<runtime::i64gt_s>;
    static constexpr auto i32gt_u = nilary<runtime::i32gt_u>;
    static constexpr auto i64gt_u = nilary<runtime::i64gt_u>;
    static constexpr auto i32le_s = nilary<runtime::i32le_s>;
    static constexpr auto i64le_s = nilary<runtime::i64le_s>;
    static constexpr auto i32le_u = nilary<runtime::i32le_u>;
    static constexpr auto i64le_u = nilary<runtime::i64le_u>;
    static constexpr auto i32ge_s = nilary<runtime::i32ge_s>;
    static constexpr auto i64ge_s = nilary<runtime::i64ge_s>;
    static constexpr auto i32ge_u = nilary<runtime::i32ge_u>;
    static constexpr auto i64ge_u = nilary<runtime::i64ge_u>;
    static constexpr auto f32eq = nilary<runtime::f32eq>;
    static constexpr auto f64eq = nilary<runtime::f64eq>;
    static constexpr auto f32ne = nilary<runtime::f32ne>;
    static constexpr auto f64ne = nilary<runtime::f64ne>;
    static constexpr auto f32lt = nilary<runtime::f32lt>;
    static constexpr auto f64lt = nilary<runtime::f64lt>;
    static constexpr auto f32gt = nilary<runtime::f32gt>;
    static constexpr auto f64gt = nilary<runtime::f64gt>;
    static constexpr auto f32le = nilary<runtime::f32le>;
    static constexpr auto f64le = nilary<runtime::f64le>;
    static constexpr auto f32ge = nilary<runtime::f32ge>;
    static constexpr auto f64ge = nilary<runtime::f64ge>;
    static constexpr auto i32clz = nilary<runtime::i32clz>;
    static constexpr auto i64clz = nilary<runtime::i64clz>;
    static constexpr auto i32ctz = nilary<runtime::i32ctz>;
    static constexpr auto i64ctz = nilary<runtime::i64ctz>;
    static constexpr auto i32popcnt = nilary<runtime::i32popcnt>;
    static constexpr auto i64popcnt = nilary<runtime::i64popcnt>;
    static constexpr auto i32add = nilary<runtime::i32add>;
    static constexpr auto i64add = nilary<runtime::i64add>;
    static constexpr auto i32sub = nilary<runtime::i32sub>;
    static constexpr auto i64sub = nilary<runtime::i64sub>;
    static constexpr auto i32mul = nilary<runtime::i32mul>;
    static constexpr auto i64mul = nilary<runtime::i64mul>;
    static constexpr auto i32div_s = nilary<runtime::i32div_s>;
    static constexpr auto i64div_s = nilary<runtime::i64div_s>;
    static constexpr auto i32div_u = nilary<runtime::i32div_u>;
    static constexpr auto i64div_u = nilary<runtime::i64div_u>;
    static constexpr auto i32rem_s = nilary<runtime::i32rem_s>;
    static constexpr auto i64rem_s = nilary<runtime::i64rem_s>;
    static constexpr auto i32rem_u = nilary<runtime::i32rem_u>;
    static constexpr auto i64rem_u = nilary<runtime::i64rem_u>;
    static constexpr auto i32and = nilary<runtime::i32and>;
    static constexpr auto i64and = nilary<runtime::i64and>;
    static constexpr auto i32or = nilary<runtime::i32or>;
    static constexpr auto i64or = nilary<runtime::i64or>;
    static constexpr auto i32xor = nilary<runtime::i32xor>;
    static constexpr auto i64xor = nilary<runtime::i64xor>;
    static constexpr auto i32shl = nilary<runtime::i32shl>;
    static constexpr auto i64shl = nilary<runtime::i64shl>;
    static constexpr auto i32shr_s = nilary<runtime::i32shr_s>;
    static constexpr auto i64shr_s = nilary<runtime::i64shr_s>;
    static constexpr auto i32shr_u = nilary<runtime::i32shr_u>;
    static constexpr auto i64shr_u = nilary<runtime::i64shr_u>;
    static constexpr auto i32rotl = nilary<runtime::i32rotl>;
    static constexpr auto i64rotl = nilary<runtime::i64rotl>;
    static constexpr auto i32rotr = nilary<runtime::i32rotr>;
    static constexpr auto i64rotr = nilary<runtime::i64rotr>;
    static constexpr auto f32abs = nilary<runtime::f32abs>;
    static constexpr auto f64abs = nilary<runtime::f64abs>;
    static constexpr auto f32neg = nilary<runtime::f32neg>;
    static constexpr auto f64neg = nilary<runtime::f64neg>;
    static constexpr auto f32ceil = nilary<runtime::f32ceil>;
    static constexpr auto f64ceil = nilary<runtime::f64ceil>;
    static constexpr auto f32floor = nilary<runtime::f32floor>;
    static constexpr auto f64floor = nilary<runtime::f64floor>;
    static constexpr auto f32trunc = nilary<runtime::f32trunc>;
    static constexpr auto f64trunc = nilary<runtime::f64trunc>;
    static constexpr auto f32nearest = nilary<runtime::f32nearest>;
    static constexpr auto f64nearest = nilary<runtime::f64nearest>;
    static constexpr auto f32sqrt = nilary<runtime::f32sqrt>;
    static constexpr auto f64sqrt = nilary<runtime::f64sqrt>;
    static constexpr auto f32add = nilary<runtime::f32add>;
    static constexpr auto f64add = nilary<runtime::f64add>;
    static constexpr auto f32sub = nilary<runtime::f32sub>;
    static constexpr auto f64sub = nilary<runtime::f64sub>;
    static constexpr auto f32mul = nilary<runtime::f32mul>;
    static constexpr auto f64mul = nilary<runtime::f64mul>;
    static constexpr auto f32div = nilary<runtime::f32div>;
    static constexpr auto f64div = nilary<runtime::f64div>;
    static constexpr auto f32min = nilary<runtime::f32min>;
    static constexpr auto f64min = nilary<runtime::f64min>;
    static constexpr auto f32max = nilary<runtime::f32max>;
    static constexpr auto f64max = nilary<runtime::f64max>;
    static constexpr auto f32copysign = nilary<runtime::f32copysign>;
    static constexpr auto f64copysign = nilary<runtime::f64copysign>;
    static constexpr auto i32wrap_i64 = nilary<runtime::i32wrap_i64>;
    static constexpr auto i64extend_i32_s = nilary<runtime::i64extend_i32_s>;
    static constexpr auto i64extend_i32_u = nilary<runtime::i64extend_i32_u>;
    static constexpr auto i32trunc_f32_s = nilary<runtime::i32trunc_f32_s>;
    static constexpr auto i64trunc_f32_s = nilary<runtime::i64trunc_f32_s>;
    static constexpr auto i32trunc_f32_u = nilary<runtime::i32trunc_f32_u>;
    static constexpr auto i64trunc_f32_u = nilary<runtime::i64trunc_f32_u>;
    static constexpr auto i32trunc_f64_s = nilary<runtime::i32trunc_f64_s>;
    static constexpr auto i64trunc_f64_s = nilary<runtime::i64trunc_f64_s>;
    static constexpr auto i32trunc_f64_u = nilary<runtime::i32trunc_f64_u>;
    static constexpr auto i64trunc_f64_u = nilary<runtime::i64trunc_f64_u>;
    static constexpr auto f32convert_i32_s = nilary<runtime::f32convert_i32_s>;
    static constexpr auto f64convert_i32_s = nilary<runtime::f64convert_i32_s>;
    static constexpr auto f32convert_i32_u = nilary<runtime::f32convert_i32_u>;
    static constexpr auto f64convert_i32_u = nilary<runtime::f64convert_i32_u>;
    static constexpr auto f32convert_i64_s = nilary<runtime::f32convert_i64_s>;
    static constexpr auto f64convert_i64_s = nilary<runtime::f64convert_i64_s>;
    static constexpr auto f32convert_i64_u = nilary<runtime::f32convert_i64_u>;
    static constexpr auto f64convert_i64_u = nilary<runtime::f64convert_i64_u>;
    static constexpr auto f32demote_f64 = nilary<runtime::f32demote_f64>;
    static constexpr auto f64promote_f32 = nilary<runtime::f64promote_f32>;
    static void i32reinterpret_f32(SHARED_PARAMS) {}
    static void f32reinterpret_i32(SHARED_PARAMS) {}
    static void i64reinterpret_f64(SHARED_PARAMS) {}
    static void f64reinterpret_i64(SHARED_PARAMS) {}
    static constexpr auto i32extend8_s = nilary<runtime::i32extend8_s>;
    static constexpr auto i32extend16_s = nilary<runtime::i32extend16_s>;
    static constexpr auto i64extend8_s = nilary<runtime::i64extend8_s>;
    static constexpr auto i64extend16_s = nilary<runtime::i64extend16_s>;
    static constexpr auto i64extend32_s = nilary<runtime::i64extend32_s>;
    static constexpr auto ref_null = nilary<runtime::ref_null>;
    static constexpr auto ref_is_null = nilary<runtime::ref_is_null>;
    static constexpr auto ref_func = unary<runtime::ref_func>;
    static constexpr auto ref_eq = nilary<runtime::ref_eq>;
    static constexpr auto i32_trunc_sat_f32_s =
        nilary<runtime::i32_trunc_sat_f32_s>;
    static constexpr auto i32_trunc_sat_f32_u =
        nilary<runtime::i32_trunc_sat_f32_u>;
    static constexpr auto i32_trunc_sat_f64_s =
        nilary<runtime::i32_trunc_sat_f64_s>;
    static constexpr auto i32_trunc_sat_f64_u =
        nilary<runtime::i32_trunc_sat_f64_u>;
    static constexpr auto i64_trunc_sat_f32_s =
        nilary<runtime::i64_trunc_sat_f32_s>;
    static constexpr auto i64_trunc_sat_f32_u =
        nilary<runtime::i64_trunc_sat_f32_u>;
    static constexpr auto i64_trunc_sat_f64_s =
        nilary<runtime::i64_trunc_sat_f64_s>;
    static constexpr auto i64_trunc_sat_f64_u =
        nilary<runtime::i64_trunc_sat_f64_u>;
    static constexpr auto memory_init = unary<runtime::memory_init>;
    static constexpr auto data_drop = unary<runtime::data_drop>;
    static constexpr auto memory_copy = nilary<runtime::memory_copy>;
    static constexpr auto memory_fill = nilary<runtime::memory_fill>;
    static constexpr auto table_init = binary<runtime::table_init>;
    static constexpr auto elem_drop = unary<runtime::elem_drop>;
    static constexpr auto table_copy = binary<runtime::table_copy>;
    static constexpr auto table_grow = unary<runtime::table_grow>;
    static constexpr auto table_size = unary<runtime::table_size>;
    static constexpr auto table_fill = unary<runtime::table_fill>;
};

} // namespace mitey