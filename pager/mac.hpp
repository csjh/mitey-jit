#pragma once

#include "executable.hpp"
#include <libkern/OSCacheControl.h>
#include <sys/mman.h>
#include <unistd.h>

namespace mitey {
class MacExecutable {
  public:
    static Allocation allocate(uint32_t size) {
        if (size == 0)
            return Allocation(nullptr, [](auto) {});

        auto ptr = reinterpret_cast<uint8_t *>(
            mmap(nullptr, size, PROT_READ | PROT_WRITE | PROT_EXEC,
                 MAP_PRIVATE | MAP_ANONYMOUS | MAP_JIT, -1, 0));

        if (ptr == MAP_FAILED || ptr == nullptr) {
            throw std::bad_alloc();
        }

        auto alloc = Allocation(ptr + sizeof(uint32_t), [](uint8_t *ptr) {
            ptr -= sizeof(uint32_t);
            auto size = *reinterpret_cast<uint32_t *>(ptr);
            munmap(ptr, size);
        });

        write(alloc, [&] {
            std::memcpy(ptr, &size, sizeof(uint32_t));
            return 0;
        });

        return alloc;
    }

    static void write(const Allocation &alloc,
                      const std::function<size_t()> &cb) {
        if (!alloc)
            return;

        pthread_jit_write_protect_np(false);
        auto written = cb();
        pthread_jit_write_protect_np(true);

        // flush instruction cache to ensure code is executable
        sys_icache_invalidate(alloc.get(), written);
    }
};
} // namespace mitey