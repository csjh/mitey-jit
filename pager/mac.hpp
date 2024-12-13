#pragma once

#include "executable.hpp"
#include <libkern/OSCacheControl.h>
#include <sys/mman.h>
#include <unistd.h>

namespace mitey {
class MacExecutable : public Executable {
  public:
    Allocation allocate(size_t size) override {
        auto ptr = mmap(nullptr, size, PROT_READ | PROT_WRITE | PROT_EXEC,
                        MAP_PRIVATE | MAP_ANONYMOUS | MAP_JIT, -1, 0);

        if (ptr == MAP_FAILED || ptr == nullptr) {
            throw std::bad_alloc();
        }

        return Allocation(ptr, size);
    }

    void deallocate(Allocation alloc) override {
        munmap(alloc.ptr, alloc.size);
    }

    void write(Allocation alloc, std::function<void()> cb) override {
        pthread_jit_write_protect_np(false);
        cb();
        pthread_jit_write_protect_np(true);

        // flush instruction cache to ensure code is executable
        sys_icache_invalidate(alloc.ptr, alloc.size);
    }
};
} // namespace mitey