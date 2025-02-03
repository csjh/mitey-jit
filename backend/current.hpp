// chooses the architecture based on current computer
// macros from https://sourceforge.net/p/predef/wiki/Architectures/
#if defined(__aarch64__)
#include "./arm64.hpp"
#elif defined(__x86_64__)
#error "x86 is not supported"
#else
#error "unsupported architecture"
#endif
