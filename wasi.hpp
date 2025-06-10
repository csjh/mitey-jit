#include <csetjmp>
#include <cstddef>
#include <stdint.h>

#define __WASM_MEMORY std::byte *memory

typedef uint32_t wasm_size_t;

typedef struct {
    uint32_t v;
} wasm_ptr_t;

typedef struct {
    wasm_ptr_t buf;
    wasm_size_t buf_len;
} iovec;

typedef struct {
    uint8_t fs_filetype;
    uint32_t fs_flags;
    uint64_t fs_rights_base;
    uint64_t fs_rights_inheriting;
} fd_fdstat_t;

typedef struct {
    uint64_t dev;
    uint64_t ino;
    uint8_t filetype;
    uint64_t nlink;
    uint64_t size;
    uint64_t atim;
    uint64_t mtim;
    uint64_t ctim;
} filestat_t;

#ifdef __cplusplus
extern "C" {
#endif

int32_t register_preopen(int32_t fd, const char *path);

void set_args(int argc, const char **argv);

// File descriptor write
int32_t fd_write(__WASM_MEMORY, // non-standard: wasm memory passed in
                 int32_t fd,
                 const iovec *iovs, // Array of iovec structs
                 wasm_size_t iovs_len,
                 wasm_size_t *nwritten // Bytes written
);

int32_t fd_sync(int32_t fd);

// Command line arguments
int32_t args_get(__WASM_MEMORY, wasm_ptr_t *argv, // Buffer to store args
                 char *argv_buf                   // Buffer for arg string data
);

int32_t args_sizes_get(
    wasm_size_t *argc,         // Number of arguments
    wasm_size_t *argv_buf_size // Total size needed for argument string data
);

// Environment variables
int32_t environ_get(__WASM_MEMORY,
                    char **environ,   // Buffer to store environment vars
                    char *environ_buf // Buffer for environment string data
);

int32_t environ_sizes_get(
    wasm_size_t *environ_count, // Number of environment variables
    wasm_size_t
        *environ_buf_size // Total size needed for environment string data
);

// Clock operations
int32_t clock_res_get(int32_t clock_id,    // Clock identifier
                      uint64_t *resolution // Clock resolution in nanoseconds
);

int32_t clock_time_get(int32_t clock_id,   // Clock identifier
                       uint64_t precision, // Maximum tolerable error
                       uint64_t *time      // Current time in nanoseconds
);

// File operations
int32_t fd_close(int32_t fd // File descriptor to close
);

int32_t fd_fdstat_get(int32_t fd, // File descriptor
                      void *stat  // Buffer for stat structure
);

int32_t fd_fdstat_set_flags(int32_t fd,   // File descriptor
                            int32_t flags // New flags
);

int32_t fd_filestat_get(int32_t fd, // File descriptor
                        void *buf   // Buffer for filestat structure
);

// Preopen operations
int32_t fd_prestat_get(int32_t fd, // File descriptor
                       void *buf   // Buffer for prestat structure
);

int32_t fd_prestat_dir_name(int32_t fd,          // File descriptor
                            char *path,          // Buffer for directory name
                            wasm_size_t path_len // Length of buffer
);

// File reading
int32_t fd_read(__WASM_MEMORY,        // non-standard: wasm memory passed in
                int32_t fd,           // File descriptor
                const iovec *iovs,    // Array of iovec structs
                wasm_size_t iovs_len, // Number of iovecs
                wasm_size_t *nread    // Bytes read
);

// Directory operations
int32_t fd_readdir(int32_t fd,          // Directory file descriptor
                   void *buf,           // Buffer for directory entries
                   wasm_size_t buf_len, // Buffer length
                   uint64_t cookie,     // Starting position
                   wasm_size_t *bufused // Bytes used in buffer
);

// File positioning
int32_t fd_seek(int32_t fd,         // File descriptor
                int64_t offset,     // Offset
                int32_t whence,     // Starting position (SEEK_SET, etc)
                uint64_t *newoffset // New offset after seek
);

// Path operations
int32_t path_filestat_get(int32_t fd,           // Base directory FD
                          int32_t flags,        // Flags
                          const char *path,     // Path
                          wasm_size_t path_len, // Path length
                          void *buf             // Buffer for filestat
);

int32_t path_open(int32_t fd,                    // Base directory FD
                  int32_t dirflags,              // Directory flags
                  const char *path,              // Path
                  wasm_size_t path_len,          // Path length
                  int32_t oflags,                // Open flags
                  uint64_t fs_rights_base,       // File rights
                  uint64_t fs_rights_inheriting, // Inherited rights
                  int32_t fdflags,               // File descriptor flags
                  int32_t *opened_fd             // Resulting file descriptor
);

int32_t path_remove_directory(int32_t fd,          // Base directory FD
                              const char *path,    // Path
                              wasm_size_t path_len // Path length
);

int32_t path_unlink_file(int32_t fd,          // Base directory FD
                         const char *path,    // Path
                         wasm_size_t path_len // Path length
);

extern std::jmp_buf proc_buf;
extern int32_t proc_status;

// Process control
void proc_exit(int32_t rval // Exit code
);

// Random number generation
int32_t random_get(void *buf,          // Buffer to fill with random bytes
                   wasm_size_t buf_len // Buffer length
);

int32_t poll_oneoff(__WASM_MEMORY,  // non-standard: wasm memory passed in
                    const char *in, // Input buffer
                    char *out,      // Output buffer
                    wasm_size_t nsubscriptions, // Number of subscriptions
                    wasm_size_t *nevents        // Number of events
);

int32_t fd_tell(__WASM_MEMORY,   // non-standard: wasm memory passed in
                int32_t fd,      // File descriptor
                wasm_size_t *out // Output position
);

int32_t path_readlink(__WASM_MEMORY, int32_t fd, char *path,
                      wasm_size_t path_len, char *buf, wasm_size_t buf_len,
                      wasm_size_t *read_len);

int32_t path_symlink(__WASM_MEMORY, char *old_path, wasm_size_t old_path_len,
                     int32_t fd, char *new_path, wasm_size_t new_path_len);

int32_t path_rename(__WASM_MEMORY, int32_t old_fd, char *old_path,
                    wasm_size_t old_path_len, int32_t new_fd, char *new_path,
                    wasm_size_t new_path_len);

#ifdef __cplusplus
} // extern "C"
#endif
