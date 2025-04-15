#include "./backend/arm64/baseline.hpp"
#include "./wasi.hpp"
#include "interfacing.hpp"
#include "module.hpp"
#include "pager/mac.hpp"
#include <fcntl.h>

using namespace mitey;

uint64_t clock_ms() {
    return std::chrono::duration_cast<std::chrono::milliseconds>(
               std::chrono::system_clock::now().time_since_epoch())
        .count();
}

int main(int argc, const char **argv) {
    if (argc < 2) {
        printf("Usage: %s <filename>\n", argv[0]);
        return 1;
    }

    auto filename = argv[1];

    auto file = fopen(filename, "rb");
    if (!file) {
        printf("Could not open file %s\n", filename);
        return 1;
    }

    fseek(file, 0, SEEK_END);
    auto length = ftell(file);
    fseek(file, 0, SEEK_SET);

    auto bytes = std::vector<uint8_t>(length);
    fread(bytes.data(), 1, length, file);
    fclose(file);

    auto start = std::chrono::high_resolution_clock::now();
    auto mod = Module::compile<Mac, arm64::Arm64>(bytes);
    auto end = std::chrono::high_resolution_clock::now();
    printf("Compilation/validation took %fms\n",
           std::chrono::duration<float, std::milli>(end - start).count());

    auto wasi = runtime::ModuleImports{
        {"fd_write", internalize<fd_write>()},
        {"fd_sync", internalize<fd_sync>()},
        {"args_get", internalize<args_get>()},
        {"args_sizes_get", internalize<args_sizes_get>()},
        {"environ_get", internalize<environ_get>()},
        {"environ_sizes_get", internalize<environ_sizes_get>()},
        {"clock_res_get", internalize<clock_res_get>()},
        {"clock_time_get", internalize<clock_time_get>()},
        {"fd_close", internalize<fd_close>()},
        {"fd_fdstat_get", internalize<fd_fdstat_get>()},
        {"fd_fdstat_set_flags", internalize<fd_fdstat_set_flags>()},
        {"fd_filestat_get", internalize<fd_filestat_get>()},
        {"fd_prestat_get", internalize<fd_prestat_get>()},
        {"fd_prestat_dir_name", internalize<fd_prestat_dir_name>()},
        {"fd_read", internalize<fd_read>()},
        {"fd_readdir", internalize<fd_readdir>()},
        {"fd_seek", internalize<fd_seek>()},
        {"path_filestat_get", internalize<path_filestat_get>()},
        {"path_open", internalize<path_open>()},
        {"path_remove_directory", internalize<path_remove_directory>()},
        {"path_unlink_file", internalize<path_unlink_file>()},
        {"proc_exit", internalize<proc_exit>()},
        {"random_get", internalize<random_get>()},
        {"poll_oneoff", internalize<poll_oneoff>()},
        {"path_readlink", internalize<path_readlink>()},
        {"path_symlink", internalize<path_symlink>()},
        {"path_rename", internalize<path_rename>()},
        {"fd_tell", internalize<fd_tell>()},
    };
    auto imports = runtime::Imports{{"wasi_snapshot_preview1", wasi}};

    auto instance = mod->instantiate(imports);

    // remove mitey-jit arg
    set_args(argc - 1, argv + 1);

    auto path = "/tmp/jit-sandbox/";
    auto fd = open(path, O_RDONLY | O_DIRECTORY, 0666);
    register_preopen(fd, "/");

    try {
        externalize<void()>(std::get<runtime::FunctionInfo>(
            instance->get_exports().at("_start")))();
    } catch (const std::exception &e) {
        printf("Exception: %s\n", e.what());
    }

    return 0;
}
