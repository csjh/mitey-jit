#pragma once

#include "executable.hpp"
#include <libkern/OSCacheControl.h>
#include <sys/mman.h>
#include <unistd.h>

namespace mitey {
class MacExecutable : public Executable {
  public:
    Allocation allocate(uint32_t size) override {
        auto ptr = reinterpret_cast<uint8_t *>(
            mmap(nullptr, size, PROT_READ | PROT_WRITE | PROT_EXEC,
                 MAP_PRIVATE | MAP_ANONYMOUS | MAP_JIT, -1, 0));

        if (ptr == MAP_FAILED || ptr == nullptr) {
            throw std::bad_alloc();
        }

        pthread_jit_write_protect_np(false);
        *reinterpret_cast<uint32_t *>(ptr) = size;
        pthread_jit_write_protect_np(true);

        return Allocation(ptr + 4, [](uint8_t *ptr) {
            ptr -= 4;
            auto size = *reinterpret_cast<uint32_t *>(ptr);
            munmap(ptr, size);
        });
    }

    void write(const Allocation &alloc,
               const std::function<void()> &cb) override {
        pthread_jit_write_protect_np(false);
        cb();
        pthread_jit_write_protect_np(true);

        auto ptr = alloc.get() - 4;

        // flush instruction cache to ensure code is executable
        sys_icache_invalidate(ptr, *reinterpret_cast<uint32_t *>(ptr));
    }
};
} // namespace mitey