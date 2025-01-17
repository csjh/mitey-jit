#include <stdint.h>

typedef uint32_t wasm_size_t;

#ifdef __cplusplus
extern "C" {
#endif

void set_args(int argc, const char **argv);

// File descriptor write
int32_t fd_write(int32_t fd,
                 const void *iovs, // Array of iovec structs
                 wasm_size_t iovs_len,
                 wasm_size_t *nwritten // Bytes written
);

// Command line arguments
int32_t args_get(char **argv,   // Buffer to store args
                 char *argv_buf // Buffer for arg string data
);

int32_t args_sizes_get(
    wasm_size_t *argc,         // Number of arguments
    wasm_size_t *argv_buf_size // Total size needed for argument string data
);

// Environment variables
int32_t environ_get(char **environ,   // Buffer to store environment vars
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
int32_t fd_read(int32_t fd,           // File descriptor
                const void *iovs,     // Array of iovec structs
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

// Process control
void proc_exit(int32_t rval // Exit code
);

// Random number generation
int32_t random_get(void *buf,          // Buffer to fill with random bytes
                   wasm_size_t buf_len // Buffer length
);

#ifdef __cplusplus
} // extern "C"
#endif
