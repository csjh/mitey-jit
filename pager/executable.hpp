#pragma once

#include <cstdint>
#include <functional>

namespace mitey {

using Allocation = std::unique_ptr<uint8_t[], void (*)(uint8_t[])>;

struct Executable {

    virtual Allocation allocate(uint32_t) = 0;

    // within the callback, the memory is guaranteed to be writable
    virtual void write(const Allocation &, const std::function<void()> &) = 0;

    virtual ~Executable() = default;
};

} // namespace mitey