#pragma once

#include <cstdint>
#include <memory>

namespace mitey {

using Allocation = std::unique_ptr<uint8_t[], void (*)(uint8_t[])>;

enum class AllocationKind {
    Heap,
    Stack,
    Executable,
};

} // namespace mitey