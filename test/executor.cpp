#include "../backend/arm64.hpp"
#include "../instance.hpp"
#include "../interfacing.hpp"
#include "../module.hpp"
#include "../pager/mac.hpp"
#include "json.hpp"
#include <filesystem>
#include <fstream>
#include <iostream>
#include <stdexcept>
#include <string>
#include <system_error>
#include <variant>
#include <vector>

using namespace mitey;
using namespace mitey::runtime;

struct value {
    std::string type;
    std::string value;
};

NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(value, type, value)

std::vector<WasmValue> to_wasm_values(const std::vector<value> &values) {
    static std::unordered_map<std::string, void *> externrefs;

    std::vector<WasmValue> result;
    for (auto &v : values) {
        if (v.type == "i32") {
            result.push_back(
                WasmValue(static_cast<uint32_t>(std::stoul(v.value))));
        } else if (v.type == "i64") {
            result.push_back(WasmValue(std::stoull(v.value)));
        } else if (v.type == "f32") {
            if (v.value == "nan:canonical") {
                result.push_back(
                    WasmValue(std::numeric_limits<float>::quiet_NaN()));
            } else if (v.value == "nan:arithmetic") {
                result.push_back(
                    WasmValue(std::numeric_limits<float>::signaling_NaN()));
            } else {
                uint32_t bytes = std::stoul(v.value);
                float value;
                std::memcpy(&value, &bytes, sizeof(float));
                result.push_back(WasmValue(value));
            }
        } else if (v.type == "f64") {
            if (v.value == "nan:canonical") {
                result.push_back(
                    WasmValue(std::numeric_limits<double>::quiet_NaN()));
            } else if (v.value == "nan:arithmetic") {
                result.push_back(
                    WasmValue(std::numeric_limits<double>::signaling_NaN()));
            } else {
                uint64_t bytes = std::stoull(v.value);
                double value;
                std::memcpy(&value, &bytes, sizeof(double));
                result.push_back(WasmValue(value));
            }
        } else if (v.type == "externref") {
            if (v.value == "null") {
                result.push_back(WasmValue((void *)nullptr));
            } else {
                if (externrefs.find(v.value) == externrefs.end()) {
                    externrefs[v.value] = &externrefs[v.value];
                }
                result.push_back(WasmValue(externrefs[v.value]));
            }
        } else if (v.type == "funcref") {
            if (v.value == "null") {
                result.push_back(WasmValue((void *)nullptr));
            } else {
                std::cerr << "Unknown reference value: " << v.value
                          << std::endl;
            }
        } else {
            std::cerr << "Unknown type: " << v.type << std::endl;
        }
    }
    return result;
}

struct only_type {
    std::string type;
};

NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(only_type, type)

struct get_action {
    std::string module;
    std::string field;
};

namespace nlohmann {
template <> struct adl_serializer<get_action> {
    static void to_json(json &j, const get_action &opt) {
        j = json{{"module", opt.module}, {"field", opt.field}};
    }

    static void from_json(const json &j, get_action &opt) {
        if (j.contains("module")) {
            opt.module = j["module"];
        } else {
            opt.module = "default";
        }
        opt.field = j["field"];
    }
};
} // namespace nlohmann

struct invoke_action {
    std::string module;
    std::string field;
    std::vector<value> args;
};

namespace nlohmann {
template <> struct adl_serializer<invoke_action> {
    static void to_json(json &j, const invoke_action &opt) {
        j = json{
            {"module", opt.module}, {"field", opt.field}, {"args", opt.args}};
    }

    static void from_json(const json &j, invoke_action &opt) {
        if (j.contains("module")) {
            opt.module = j["module"];
        } else {
            opt.module = "default";
        }
        opt.field = j["field"];
        opt.args = j["args"];
    }
};
} // namespace nlohmann

using act = std::variant<get_action, invoke_action>;

namespace nlohmann {
template <> struct adl_serializer<act> {
    static void to_json(json &j, const act &opt) {
        std::visit([&j](auto &&arg) { j = arg; }, opt);
    }

    static void from_json(const json &j, act &opt) {
        if (j["type"] == "get") {
            opt = j.get<get_action>();
        } else if (j["type"] == "invoke") {
            opt = j.get<invoke_action>();
        } else {
            std::cerr << "Unknown action type: " << j["type"] << std::endl;
        }
    }
};
} // namespace nlohmann

struct test_module {
    std::string type;
    int line;
    std::string name;
    std::string filename;
};

namespace nlohmann {
template <> struct adl_serializer<test_module> {
    static void to_json(json &j, const test_module &opt) {
        j = json{{"type", opt.type},
                 {"line", opt.line},
                 {"name", opt.name},
                 {"filename", opt.filename}};
    }

    static void from_json(const json &j, test_module &opt) {
        opt.type = j["type"];
        opt.line = j["line"];
        opt.filename = j["filename"];
        if (j.contains("name")) {
            opt.name = j["name"];
        } else {
            opt.name = "default";
        }
    }
};
} // namespace nlohmann

struct test_malformed {
    std::string type;
    int line;
    std::string filename;
    std::string text;
    std::string module_type;
};

NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(test_malformed, type, line, filename, text,
                                   module_type)

struct test_unlinkable {
    std::string type;
    int line;
    std::string filename;
    std::string text;
    std::string module_type;
};

NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(test_unlinkable, type, line, filename, text,
                                   module_type)

struct test_register {
    std::string type;
    int line;
    std::string name;
    std::string as;
};

namespace nlohmann {
template <> struct adl_serializer<test_register> {
    static void to_json(json &j, const test_register &opt) {
        j = json{{"type", opt.type},
                 {"line", opt.line},
                 {"as", opt.as},
                 {"name", opt.name}};
    }

    static void from_json(const json &j, test_register &opt) {
        opt.type = j["type"];
        opt.line = j["line"];
        if (j.contains("name")) {
            opt.name = j["name"];
        } else {
            opt.name = "default";
        }
        opt.as = j["as"];
    }
};
} // namespace nlohmann

struct test_return {
    std::string type;
    int line;
    act action;
    std::vector<value> expected;
};

NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(test_return, type, line, action, expected)

struct test_action {
    std::string type;
    int line;
    act action;
    std::vector<value> expected;
};

NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(test_action, type, line, action, expected)

struct test_invalid {
    std::string type;
    int line;
    std::string filename;
    std::string text;
    std::string module_type;
};

NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(test_invalid, type, line, filename, text,
                                   module_type)

struct test_trap {
    std::string type;
    int line;
    act action;
    std::string text;
    std::vector<only_type> expected;
};

NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(test_trap, type, line, action, text,
                                   expected)

struct test_exhaustion {
    std::string type;
    int line;
    act action;
    std::string text;
    std::vector<only_type> expected;
};

NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(test_exhaustion, type, line, action, text,
                                   expected)

struct test_uninstantiable {
    std::string type;
    int line;
    std::string filename;
    std::string text;
    std::string module_type;
};

NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(test_uninstantiable, type, line, filename,
                                   text, module_type)

using Tests = std::variant<test_module, test_malformed, test_unlinkable,
                           test_return, test_action, test_invalid, test_trap,
                           test_exhaustion, test_register, test_uninstantiable>;

namespace nlohmann {
template <> struct adl_serializer<Tests> {
    static void to_json(json &j, const Tests &opt) {
        std::visit([&j](auto &&arg) { j = arg; }, opt);
    }

    static void from_json(const json &j, Tests &opt) {
        if (j["type"] == "module") {
            opt = j.get<test_module>();
        } else if (j["type"] == "assert_return") {
            opt = j.get<test_return>();
        } else if (j["type"] == "assert_malformed") {
            opt = j.get<test_malformed>();
        } else if (j["type"] == "assert_invalid") {
            opt = j.get<test_invalid>();
        } else if (j["type"] == "assert_unlinkable") {
            opt = j.get<test_unlinkable>();
        } else if (j["type"] == "assert_trap") {
            opt = j.get<test_trap>();
        } else if (j["type"] == "assert_exhaustion") {
            opt = j.get<test_exhaustion>();
        } else if (j["type"] == "action") {
            opt = j.get<test_action>();
        } else if (j["type"] == "register") {
            opt = j.get<test_register>();
        } else if (j["type"] == "assert_uninstantiable") {
            opt = j.get<test_uninstantiable>();
        } else {
            std::cerr << "Unknown type: " << j["type"] << std::endl;
        }
    }
};
} // namespace nlohmann

struct wastjson {
    std::string source_filename;
    std::vector<Tests> commands;
};

NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(wastjson, source_filename, commands)

std::shared_ptr<mitey::Instance>
from_file(const std::string &filename, const mitey::runtime::Imports &imports) {
    std::ifstream wasm_file{filename, std::ios::binary};
    if (!wasm_file) {
        throw std::system_error(errno, std::system_category(), filename);
    }

    wasm_file.seekg(0, std::ios::end);
    long length = wasm_file.tellg();
    wasm_file.seekg(0, std::ios::beg);

    // load data into a unique_ptr
    auto bytes = std::vector<uint8_t>(length);

    wasm_file.read(reinterpret_cast<char *>(bytes.data()), length);
    wasm_file.close();

    auto module = mitey::Module::compile<mitey::Mac, mitey::Arm64>(bytes);
    auto instance = module->instantiate(imports);

    // force all created instances to stick around
    // for export usage
    static std::vector<std::shared_ptr<Instance>> keepalive;
    keepalive.push_back(instance);

    return instance;
}

namespace fs = std::filesystem;
fs::path resolve_relative(const fs::path &file1, const fs::path &file2) {
    return fs::absolute(file1.parent_path() / file2).lexically_normal();
}

void print() { std::cout << "spectest print" << std::endl; }

void print_i32(uint32_t v) {
    std::cout << "spectest print_i32: " << v << std::endl;
}

void print_i64(uint64_t v) {
    std::cout << "spectest print_i64: " << v << std::endl;
}

void print_f32(float v) {
    std::cout << "spectest print_f32: " << v << std::endl;
}

void print_f64(double v) {
    std::cout << "spectest print_f64: " << v << std::endl;
}

void print_i32_f32(uint32_t i, float f) {
    std::cout << "spectest print_i32_f32: " << i << " " << f << std::endl;
}

void print_f64_f64(double d1, double d2) {
    std::cout << "spectest print_f64_f64: " << d1 << " " << d2 << std::endl;
}

int main(int argv, char **argc) {
    if (argv != 2) {
        std::cerr << "Usage: " << argc[0] << " <filename>" << std::endl;
        return 1;
    }

    // takes a json file as input
    std::string filename = argc[1];

    // read the json file
    std::ifstream ifs{filename};
    nlohmann::json j;
    ifs >> j;

    auto wast = j.template get<wastjson>();

    std::unordered_map<std::string, std::shared_ptr<mitey::Instance>> instances;
    mitey::runtime::Exports spectest{
        {"global_i32",
         std::make_shared<WasmGlobal>(valtype::i32, mut::const_, 666u)},
        {"global_i64",
         std::make_shared<WasmGlobal>(valtype::i64, mut::const_, 666ull)},
        {"global_f32",
         std::make_shared<WasmGlobal>(valtype::f32, mut::const_, 666.6f)},
        {"global_f64",
         std::make_shared<WasmGlobal>(valtype::f64, mut::const_, 666.6)},
        {"table", std::make_shared<WasmTable>(valtype::funcref, 10, 20)},
        {"memory",
         std::make_shared<WasmMemory>(1, 2, Mac::allocate, Mac::grow)},
        {"print", internalize<print>()},
        {"print_i32", internalize<print_i32>()},
        {"print_i64", internalize<print_i64>()},
        {"print_f32", internalize<print_f32>()},
        {"print_f64", internalize<print_f64>()},
        {"print_i32_f32", internalize<print_i32_f32>()},
        {"print_f64_f64", internalize<print_f64_f64>()}};

    Imports imports{{"spectest", spectest}};

    auto execute_action = [&](const act &a) {
        if (std::holds_alternative<get_action>(a)) {
            auto &action = std::get<get_action>(a);
            return std::vector{
                std::get<std::shared_ptr<WasmGlobal>>(
                    instances[action.module]->get_exports().at(action.field))
                    ->value};
        } else if (std::holds_alternative<invoke_action>(a)) {
            auto &action = std::get<invoke_action>(a);
            return externalize(std::get<FunctionInfo>(
                instances[action.module]->get_exports().at(action.field)))(
                to_wasm_values(action.args));
        } else {
            throw std::runtime_error("Unknown action type");
        }
    };

    uint32_t passes = 0, soft_passes = 0, failures = 0;

    auto runtime_error = [&]<typename T>(const auto &m) {
        try {
            execute_action(m.action);

            std::cerr << "Expected " << typeid(T).name() << " for test"
                      << std::endl;
            failures++;
        } catch (const T &e) {
            std::string what = e.what();
            if (!what.starts_with(m.text) && !m.text.starts_with(what)) {
                std::cerr << "Expected error message: " << m.text
                          << " but got: " << e.what() << std::endl;
                soft_passes++;
            } else {
                passes++;
            }
        } catch (std::runtime_error &e) {
            std::cerr << "Expected " << typeid(T).name()
                      << " with message: " << m.text << " but got: " << e.what()
                      << std::endl;
            soft_passes++;
        } catch (...) {
            std::cerr << "Expected " << typeid(T).name() << " for test"
                      << std::endl;
            failures++;
        }
    };

    auto compile_error = [&]<typename T>(const auto &m) {
        try {
            from_file(resolve_relative(filename, m.filename), imports);

            std::cerr << "Expected " << typeid(T).name()
                      << " for file: " << m.filename << std::endl;
            failures++;
        } catch (const T &e) {
            std::string what = e.what();
            if (!what.starts_with(m.text) && !m.text.starts_with(what)) {
                std::cerr << "Expected error message: " << m.text
                          << " but got: " << e.what() << std::endl;
                soft_passes++;
            } else {
                passes++;
            }
        } catch (std::runtime_error &e) {
            std::cerr << "Expected " << typeid(T).name()
                      << " with message: " << m.text << " but got: " << e.what()
                      << std::endl;
            soft_passes++;
        }
    };

    for (auto &t : wast.commands) {
        nlohmann::json j = t;
        std::cerr << "Running test: " << j << std::endl;

        if (std::holds_alternative<test_module>(t)) {
            auto &m = std::get<test_module>(t);
            auto instance =
                from_file(resolve_relative(filename, m.filename), imports);
            instances[m.name] = instance;
            instances["default"] = instance;
            passes++;
        } else if (std::holds_alternative<test_return>(t)) {
            auto &m = std::get<test_return>(t);

            std::vector<WasmValue> result = execute_action(m.action);

            if (result.size() != m.expected.size()) {
                std::cerr << "Expected " << m.expected.size()
                          << " results but got " << result.size() << std::endl;
                failures++;
                continue;
            }

            auto expected_results = to_wasm_values(m.expected);

            auto test = [](std::string &ty, WasmValue &res, WasmValue &exp) {
                if (ty == "i32" && res.i32 != exp.i32) {
                    std::cerr << "Expected: " << exp.i32
                              << " but got: " << res.i32 << std::endl;
                    return false;
                } else if (ty == "i64" && res.i64 != exp.i64) {
                    std::cerr << "Expected: " << exp.i64
                              << " but got: " << res.i64 << std::endl;
                    return false;
                } else if (ty == "f32" && (res.f32 != exp.f32) &&
                           (std::isnan(res.f32) != std::isnan(exp.f32))) {
                    std::cerr << "Expected: " << exp.f32
                              << "(isnan: " << std::isnan(exp.f32) << ")"
                              << " but got: " << res.f32
                              << "(isnan: " << std::isnan(res.f32) << ")"
                              << std::endl;
                    return false;
                } else if (ty == "f64" && (res.f64 != exp.f64) &&
                           (std::isnan(res.f64) != std::isnan(exp.f64))) {
                    std::cerr << "Expected: " << exp.f64
                              << "(isnan: " << std::isnan(exp.f64) << ")"
                              << " but got: " << res.f64
                              << "(isnan: " << std::isnan(res.f64) << ")"
                              << std::endl;
                    return false;
                }
                return true;
            };

            bool failed = false;
            for (uint32_t i = 0; i < result.size(); ++i) {
                if (!test(m.expected[i].type, result[i], expected_results[i])) {
                    failed = true;
                    break;
                }
            }
            if (failed) {
                failures++;
            } else {
                passes++;
            }
        } else if (std::holds_alternative<test_action>(t)) {
            auto &m = std::get<test_action>(t);
            assert(m.expected.size() == 0);
            execute_action(m.action);
            passes++;
        } else if (std::holds_alternative<test_malformed>(t)) {
            auto &m = std::get<test_malformed>(t);
            if (m.module_type != "binary")
                continue;
            compile_error.template operator()<malformed_error>(m);
        } else if (std::holds_alternative<test_invalid>(t)) {
            auto &m = std::get<test_invalid>(t);
            compile_error.template operator()<validation_error>(m);
        } else if (std::holds_alternative<test_unlinkable>(t)) {
            auto &m = std::get<test_unlinkable>(t);
            compile_error.template operator()<link_error>(m);
        } else if (std::holds_alternative<test_uninstantiable>(t)) {
            auto &m = std::get<test_uninstantiable>(t);
            compile_error.template operator()<uninstantiable_error>(m);
        } else if (std::holds_alternative<test_trap>(t)) {
            auto &m = std::get<test_trap>(t);
            runtime_error.template operator()<trap_error>(m);
        } else if (std::holds_alternative<test_exhaustion>(t)) {
            auto &m = std::get<test_exhaustion>(t);
            runtime_error.template operator()<trap_error>(m);
        } else if (std::holds_alternative<test_register>(t)) {
            auto &m = std::get<test_register>(t);
            imports[m.as] = instances[m.name]->get_exports();
        } else {
            std::cerr << "Unhandled std::variant type" << std::endl;
        }
    }

    std::cout << "Passes: " << passes << std::endl;
    std::cout << "Soft passes: " << soft_passes << std::endl;
    std::cout << "Failures: " << failures << std::endl;

    return 0;
}
