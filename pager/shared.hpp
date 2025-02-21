#pragma once

#include <cstddef>
#include <memory>

namespace mitey {

using Allocation = std::unique_ptr<std::byte[], void (*)(std::byte[])>;

enum class AllocationKind {
    Heap,
    Stack,
    Executable,
};

} // namespace mitey