#include <cstddef>
#include <functional>

struct Allocation {
    void *ptr;
    size_t size;
};

struct Executable {
    virtual Allocation allocate(size_t) = 0;
    virtual void deallocate(Allocation) = 0;

    // within the callback, the memory is guaranteed to be writable
    virtual void write(Allocation, std::function<void()>) = 0;

    virtual ~Executable() = default;
};
