#pragma once

#include "../runtime.hpp"
#include "./shared.hpp"
#include <functional>
#include <libkern/OSCacheControl.h>
#include <signal.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>

namespace mitey {

static constexpr size_t stack_size = 5 * 1024 * 1024; // 5MB
static constexpr size_t guard_size = stack_size;
static constexpr size_t total_size = stack_size + guard_size;
static uint8_t *guard_page = nullptr;

// follows the implementation described by
// https://docs.google.com/document/d/17y4kxuHFrVxAiuCP_FFtFA2HP5sNPsCD10KEx17Hz6M/edit?tab=t.0#heading=h.tbi7hpbheoai
// end - start == 16gb
struct HeapAllocation {
    uint8_t *start;
    uint8_t *heap_start;
    uint8_t *heap_end;
    uint8_t *end;
};
// |-------------- allocation --------------|
// |--- guard ---|--- heap ---|--- guard ---|

static std::vector<HeapAllocation> heaps;

static void sigbus_handler(int, siginfo_t *si, void *) {
    // check if we own the bus fault
    if (guard_page <= si->si_addr && si->si_addr < guard_page + guard_size) {
        runtime::trap(runtime::TrapKind::call_stack_exhausted);
    } else if (std::ranges::any_of(heaps, [&](auto &heap) {
                   return (heap.start <= si->si_addr &&
                           si->si_addr < heap.heap_start) ||
                          (heap.heap_end <= si->si_addr &&
                           si->si_addr < heap.end);
               })) {
        runtime::trap(runtime::TrapKind::out_of_bounds_memory_access);
    }

    // handle non-owned bus fault
    struct sigaction sa;
    sa.sa_handler = SIG_DFL;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = 0;
    sigaction(SIGBUS, &sa, nullptr);
    raise(SIGBUS);
}

class Mac {
    static Allocation null() {
        return Allocation(nullptr, [](auto) {});
    }

    static int grow_allocation(HeapAllocation &allocation, uint32_t delta) {
        size_t byte_delta = delta * runtime::WasmMemory::PAGE_SIZE;

        // assume it's checked
        if (mprotect(allocation.heap_end, byte_delta, PROT_READ | PROT_WRITE) ==
            -1) {
            return -1;
        }

        allocation.heap_end += byte_delta;

        return delta;
    }

  public:
    static Allocation allocate(size_t size, AllocationKind kind) {
        if (kind == AllocationKind::Executable) {
            if (size == 0)
                return null();

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
        } else if (kind == AllocationKind::Stack) {
            // size is ignored here which is probably not ideal
            // in my defense all this signal handler stuff seems
            // not very dynamic friendly so wtv

            if (guard_page != nullptr) {
                throw std::runtime_error("stack already allocated");
            }

            auto memory = reinterpret_cast<uint8_t *>(
                mmap(nullptr, total_size, PROT_READ | PROT_WRITE,
                     MAP_PRIVATE | MAP_ANONYMOUS, -1, 0));

            if (memory == MAP_FAILED) {
                throw std::bad_alloc();
            }

            // jump past the real stack to the guard pages
            guard_page = memory + stack_size;
            if (mprotect(guard_page, stack_size, PROT_NONE) == -1) {
                munmap(memory, total_size);
                throw std::bad_alloc();
            }

            struct sigaction sa;
            sa.sa_sigaction = sigbus_handler;
            sigemptyset(&sa.sa_mask);
            sa.sa_flags = SA_SIGINFO;

            if (sigaction(SIGBUS, &sa, nullptr) == -1) {
                munmap(memory, total_size);
                throw std::bad_alloc();
            }

            return Allocation(memory,
                              [](auto ptr) { munmap(ptr, 2 * stack_size); });
        } else if (kind == AllocationKind::Heap) {
            constexpr size_t heap_size = 16ull * 1024 * 1024 * 1024; // 16GB

            // initially, size = 0
            auto memory = reinterpret_cast<uint8_t *>(
                mmap(nullptr, heap_size, PROT_NONE, MAP_PRIVATE | MAP_ANONYMOUS,
                     -1, 0));

            if (memory == MAP_FAILED) {
                throw std::bad_alloc();
            }

            auto heap_start = memory + heap_size / 2;

            auto allocation = HeapAllocation(memory, heap_start, heap_start,
                                             memory + heap_size);
            grow_allocation(allocation, size / runtime::WasmMemory::PAGE_SIZE);

            heaps.push_back(allocation);

            return Allocation(heap_start, [](auto ptr) {
                munmap(ptr - heap_size / 2, heap_size);
            });
        } else {
            throw std::runtime_error("unknown allocation kind");
        }
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

    static int grow(runtime::WasmMemory &memory, size_t delta) {
        auto &allocation =
            *std::find_if(heaps.begin(), heaps.end(), [&](auto &heap) {
                return heap.heap_start == memory.memory.get();
            });

        return grow_allocation(allocation, delta);
    }
};
} // namespace mitey