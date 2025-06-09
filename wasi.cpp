// get error codes from
// https://github.com/WebAssembly/wasi-libc/blob/main/libc-bottom-half/headers/public/wasi/api.h

#include "./wasi.hpp"

#include <cassert>
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/fcntl.h>
#include <sys/stat.h>
#include <time.h>
#include <unistd.h>

#define PATH_MAX 4096
#define MAX_PREOPENS 32

#define assert_alignment(ptr)                                                  \
    assert(((uintptr_t)(ptr) % alignof(decltype(*(ptr)))) == 0)

typedef struct {
    uint8_t pr_type;
    uint32_t pr_name_len;
} prestat_t;

typedef struct {
    int32_t fd;
    char *path;
    size_t path_len;
} preopen_entry_t;

// Global variables for storing program state
static const char **g_argv = NULL;
static wasm_size_t g_argc = 0;
static const char **g_environ = NULL;
static wasm_size_t g_environ_count = 0;
static preopen_entry_t preopens[MAX_PREOPENS];
static size_t num_preopens = 0;

void set_args(int argc, const char **argv) {
    g_argv = argv;
    g_argc = argc;
}

int32_t register_preopen(int32_t fd, const char *path) {
    if (num_preopens >= MAX_PREOPENS) {
        return ENOMEM;
    }

    size_t path_len = strlen(path);
    char *path_copy = strdup(path);
    if (!path_copy) {
        return ENOMEM;
    }

    preopens[num_preopens].fd = fd;
    preopens[num_preopens].path = path_copy;
    preopens[num_preopens].path_len = path_len;
    num_preopens++;

    return 0;
}

// Implementation of WASI functions

int32_t fd_write(__WASM_MEMORY, int32_t fd, const iovec *iovs,
                 wasm_size_t iovs_len, wasm_size_t *nwritten) {
    assert_alignment(iovs);
    assert_alignment(nwritten);

    wasm_size_t total_written = 0;

    for (wasm_size_t i = 0; i < iovs_len; i++) {
        ssize_t written = write(fd, memory + iovs[i].buf.v, iovs[i].buf_len);
        if (written < 0) {
            return 28;
        }
        total_written += written;
    }

    *nwritten = total_written;
    return 0;
}

int32_t fd_sync(int32_t fd) {
    if (fsync(fd) != 0) {
        return 28;
    }
    return 0;
}

int32_t args_get(__WASM_MEMORY, wasm_ptr_t *argv, char *argv_buf) {
    assert_alignment(argv);

    if (!g_argv || !g_argc) {
        return 28;
    }

    for (wasm_size_t i = 0; i < g_argc; i++) {
        wasm_size_t len = strlen(g_argv[i]);
        memcpy(argv_buf, g_argv[i], len);
        argv_buf[len] = '\0';
        argv[i].v = argv_buf - (char *)memory;
        argv_buf += len + 1;
    }

    return 0;
}

int32_t args_sizes_get(wasm_size_t *argc, wasm_size_t *argv_buf_size) {
    assert_alignment(argc);
    assert_alignment(argv_buf_size);

    if (!g_argv || !g_argc) {
        return 28;
    }

    *argc = g_argc;
    wasm_size_t total_size = 0;
    for (wasm_size_t i = 0; i < g_argc; i++) {
        total_size += strlen(g_argv[i]) + 1;
    }
    *argv_buf_size = total_size;

    return 0;
}

int32_t environ_get(__WASM_MEMORY, char **environ, char *environ_buf) {
    assert_alignment(environ);

    wasm_size_t buf_offset = 0;
    for (wasm_size_t i = 0; i < g_environ_count; i++) {
        wasm_size_t len = strlen(g_environ[i]) + 1;
        memcpy(environ_buf + buf_offset, g_environ[i], len);
        environ[i] = environ_buf + buf_offset;
        buf_offset += len;
    }

    return 0;
}

int32_t environ_sizes_get(wasm_size_t *environ_count,
                          wasm_size_t *environ_buf_size) {
    assert_alignment(environ_count);
    assert_alignment(environ_buf_size);

    *environ_count = g_environ_count;
    wasm_size_t total_size = 0;
    for (wasm_size_t i = 0; i < g_environ_count; i++) {
        total_size += strlen(g_environ[i]) + 1;
    }
    *environ_buf_size = total_size;

    return 0;
}

int32_t clock_res_get(int32_t clock_id, uint64_t *resolution) {
    assert_alignment(resolution);
    assert(clock_id >= 0 && clock_id <= 3);

    clockid_t clock_id_native = clock_id == 0   ? CLOCK_REALTIME
                                : clock_id == 1 ? CLOCK_MONOTONIC
                                : clock_id == 2 ? CLOCK_PROCESS_CPUTIME_ID
                                                : CLOCK_THREAD_CPUTIME_ID;

    struct timespec res;
    if (clock_getres(clock_id_native, &res) != 0) {
        return 28;
    }

    *resolution = (uint64_t)res.tv_sec * 1000000000ULL + res.tv_nsec;
    return 0;
}

int32_t clock_time_get(int32_t clock_id, uint64_t precision, uint64_t *time) {
    assert_alignment(time);
    assert(clock_id >= 0 && clock_id <= 3);

    clockid_t clock_id_native = clock_id == 0   ? CLOCK_REALTIME
                                : clock_id == 1 ? CLOCK_MONOTONIC
                                : clock_id == 2 ? CLOCK_PROCESS_CPUTIME_ID
                                                : CLOCK_THREAD_CPUTIME_ID;

    struct timespec tp;
    if (clock_gettime(clock_id_native, &tp) != 0) {
        return 28;
    }

    *time = (uint64_t)tp.tv_sec * 1000000000ULL + tp.tv_nsec;
    return 0;
}

int32_t fd_close(int32_t fd) {
    if (close(fd) != 0) {
        return 28;
    }
    return 0;
}

int32_t fd_fdstat_get(int32_t fd, void *stat) {
    fd_fdstat_t *fdstat = (fd_fdstat_t *)stat;
    assert_alignment(fdstat);

    struct stat native_stat;

    if (fstat(fd, &native_stat) != 0) {
        return 28;
    }

    // Convert native stat to WASI fdstat
    fdstat->fs_filetype = S_ISREG(native_stat.st_mode)   ? 1
                          : S_ISDIR(native_stat.st_mode) ? 2
                                                         : 0;

    int flags = fcntl(fd, F_GETFL);
    if (flags < 0) {
        return 28;
    }

    fdstat->fs_flags = flags;
    // Rights would be determined based on file type and permissions
    fdstat->fs_rights_base = 0xFFFFFFFFFFFFFFFFULL; // All rights for example
    fdstat->fs_rights_inheriting = 0xFFFFFFFFFFFFFFFFULL;

    return 0;
}

int32_t fd_fdstat_set_flags(int32_t fd, int32_t flags) {
    if (fcntl(fd, F_SETFL, flags) < 0) {
        return 28;
    }
    return 0;
}

int32_t fd_filestat_get(int32_t fd, void *buf) {
    filestat_t *filestat = (filestat_t *)buf;
    assert_alignment(filestat);

    struct stat native_stat;

    if (fstat(fd, &native_stat) != 0) {
        return 28;
    }

    filestat->dev = native_stat.st_dev;
    filestat->ino = native_stat.st_ino;
    filestat->filetype = S_ISREG(native_stat.st_mode)   ? 4
                         : S_ISDIR(native_stat.st_mode) ? 3
                                                        : 0;
    filestat->nlink = native_stat.st_nlink;
    filestat->size = native_stat.st_size;
    filestat->atim = native_stat.st_atime * 1000000000ULL;
    filestat->mtim = native_stat.st_mtime * 1000000000ULL;
    filestat->ctim = native_stat.st_ctime * 1000000000ULL;

    return 0;
}

int32_t fd_prestat_get(int32_t fd, void *buf) {
    prestat_t *prestat = (prestat_t *)buf;
    assert_alignment(prestat);

    // Search for the fd in preopens array
    for (size_t i = 0; i < num_preopens; i++) {
        if (preopens[i].fd == fd) {
            prestat->pr_type = 0; // directory type
            prestat->pr_name_len = preopens[i].path_len;
            return 0;
        }
    }

    return 8; // File descriptor not found in preopens
}

int32_t fd_prestat_dir_name(int32_t fd, char *path, wasm_size_t path_len) {
    // Search for the fd in preopens array
    for (size_t i = 0; i < num_preopens; i++) {
        if (preopens[i].fd == fd) {
            if (path_len < preopens[i].path_len) {
                return ENOBUFS; // Buffer too small
            }

            memcpy(path, preopens[i].path, preopens[i].path_len);
            return 0;
        }
    }

    return 8; // File descriptor not found in preopens
}

int32_t fd_read(__WASM_MEMORY, int32_t fd, const iovec *iovs,
                wasm_size_t iovs_len, wasm_size_t *nread) {
    assert_alignment(iovs);
    assert_alignment(nread);

    wasm_size_t total_read = 0;

    for (wasm_size_t i = 0; i < iovs_len; i++) {
        ssize_t bytes_read = read(fd, memory + iovs[i].buf.v, iovs[i].buf_len);
        if (bytes_read < 0) {
            return 28;
        }
        total_read += bytes_read;
        if ((wasm_size_t)bytes_read < iovs[i].buf_len) {
            break; // End of file reached
        }
    }

    *nread = total_read;
    return 0;
}

int32_t fd_readdir(int32_t fd, void *buf, wasm_size_t buf_len, uint64_t cookie,
                   wasm_size_t *bufused) {
    assert_alignment(bufused);

    DIR *dir = fdopendir(fd);
    if (!dir) {
        return 28;
    }

    if (cookie > 0) {
        seekdir(dir, cookie);
    }

    wasm_size_t used = 0;
    struct dirent *entry;

    while ((entry = readdir(dir)) != NULL && used < buf_len) {
        wasm_size_t name_len = strlen(entry->d_name);
        wasm_size_t entry_size =
            sizeof(uint64_t) + sizeof(uint8_t) + sizeof(uint32_t) + name_len;

        if (used + entry_size > buf_len) {
            break;
        }

        // Write dirent data to buffer
        uint8_t *current = (uint8_t *)buf + used;
        *(uint64_t *)current = entry->d_ino;
        current += sizeof(uint64_t);
        *current++ = entry->d_type;
        *(uint32_t *)current = name_len;
        current += sizeof(uint32_t);
        memcpy(current, entry->d_name, name_len);

        used += entry_size;
    }

    closedir(dir);
    *bufused = used;
    return 0;
}

int32_t fd_seek(int32_t fd, int64_t offset, int32_t whence,
                uint64_t *newoffset) {
    assert_alignment(newoffset);

    off_t new_pos = lseek(fd, offset, whence);
    if (new_pos < 0) {
        return 28;
    }

    *newoffset = new_pos;
    return 0;
}

int32_t path_filestat_get(int32_t fd, int32_t flags, const char *path,
                          wasm_size_t path_len, void *buf) {
    char path_buf[PATH_MAX];
    if (path_len >= PATH_MAX) {
        return ENAMETOOLONG;
    }

    memcpy(path_buf, path, path_len);
    path_buf[path_len] = '\0';

    struct stat native_stat;
    if (fstatat(fd, path_buf, &native_stat,
                flags & 1 ? AT_SYMLINK_NOFOLLOW : 0) != 0) {
        return 28;
    }

    filestat_t *filestat = (filestat_t *)buf;
    filestat->dev = native_stat.st_dev;
    filestat->ino = native_stat.st_ino;
    filestat->filetype = S_ISREG(native_stat.st_mode)   ? 1
                         : S_ISDIR(native_stat.st_mode) ? 2
                                                        : 0;
    filestat->nlink = native_stat.st_nlink;
    filestat->size = native_stat.st_size;
    filestat->atim = native_stat.st_atime * 1000000000ULL;
    filestat->mtim = native_stat.st_mtime * 1000000000ULL;
    filestat->ctim = native_stat.st_ctime * 1000000000ULL;

    return 0;
}

int32_t path_open(int32_t fd, int32_t dirflags, const char *path,
                  wasm_size_t path_len, int32_t oflags, uint64_t fs_rights_base,
                  uint64_t fs_rights_inheriting, int32_t fdflags,
                  int32_t *opened_fd) {
    assert_alignment(opened_fd);

    char path_buf[PATH_MAX];
    if (path_len >= PATH_MAX) {
        return ENAMETOOLONG;
    }

    memcpy(path_buf, path, path_len);
    path_buf[path_len] = '\0';

    // insecure but idrgaf
    int flags = O_RDWR | O_CLOEXEC; // Always set O_CLOEXEC

    // Convert WASI flags to native flags
    if (oflags & 1)
        flags |= O_CREAT;
    if (oflags & 2)
        flags |= O_DIRECTORY;
    if (oflags & 4)
        flags |= O_EXCL;
    if (oflags & 8)
        flags |= O_TRUNC;

    int new_fd = openat(fd, path_buf, flags, 0666);
    if (new_fd == -1) {
        if (errno == ENOENT) {
            return 44;
        } else if (errno == ENOTDIR) {
            return 54;
        } else if (errno == EBADF) {
            return 8;
        } else {
            assert(false);
        }
    }

    *opened_fd = new_fd;
    return 0;
}

int32_t path_remove_directory(int32_t fd, const char *path,
                              wasm_size_t path_len) {
    char path_buf[PATH_MAX];
    if (path_len >= PATH_MAX) {
        return ENAMETOOLONG;
    }

    memcpy(path_buf, path, path_len);
    path_buf[path_len] = '\0';

    if (unlinkat(fd, path_buf, AT_REMOVEDIR) != 0) {
        return 28;
    }

    return 0;
}

int32_t path_unlink_file(int32_t fd, const char *path, wasm_size_t path_len) {
    char path_buf[PATH_MAX];
    if (path_len >= PATH_MAX) {
        return ENAMETOOLONG;
    }

    memcpy(path_buf, path, path_len);
    path_buf[path_len] = '\0';

    if (unlinkat(fd, path_buf, 0) != 0) {
        return 28;
    }

    return 0;
}

void proc_exit(int32_t rval) { exit(rval); }

int32_t random_get(void *buf, wasm_size_t buf_len) {
    // On a real system, this should use a cryptographically secure source
    // This is just an example implementation using /dev/urandom
    int fd = open("/dev/urandom", O_RDONLY);
    if (fd < 0) {
        return 28;
    }

    wasm_size_t total_read = 0;
    while (total_read < buf_len) {
        ssize_t bytes_read =
            read(fd, (char *)buf + total_read, buf_len - total_read);
        if (bytes_read < 0) {
            close(fd);
            return 28;
        }
        if (bytes_read == 0) {
            close(fd);
            return EIO; // Unexpected EOF from random source
        }
        total_read += bytes_read;
    }

    close(fd);
    return 0;
}

int32_t poll_oneoff(__WASM_MEMORY, const char *in, char *out,
                    wasm_size_t nsubscriptions, wasm_size_t *nevents) {
    assert_alignment(nevents);
    printf("poll_oneoff");
    return 0;
}

int32_t fd_tell(__WASM_MEMORY, int32_t fd, wasm_size_t *out) {
    assert_alignment(out);

    off_t pos = lseek(fd, 0, SEEK_CUR);
    if (pos < 0) {
        return errno;
    }
    *out = static_cast<wasm_size_t>(pos);
    return 0;
}

int32_t path_readlink(__WASM_MEMORY, int32_t fd, char *path,
                      wasm_size_t path_len, char *buf, wasm_size_t buf_len,
                      wasm_size_t *read_len) {
    assert_alignment(read_len);

    char path_buf[PATH_MAX];
    if (path_len >= PATH_MAX) {
        return ENAMETOOLONG;
    }
    memcpy(path_buf, path, path_len);
    path_buf[path_len] = '\0';

    ssize_t len = readlinkat(fd, path_buf, buf, buf_len);
    if (len < 0) {
        return errno;
    }
    *read_len = static_cast<wasm_size_t>(len);
    return 0;
}

int32_t path_symlink(__WASM_MEMORY, char *old_path, wasm_size_t old_path_len,
                     int32_t fd, char *new_path, wasm_size_t new_path_len) {
    char old_path_buf[PATH_MAX];
    if (old_path_len >= PATH_MAX) {
        return ENAMETOOLONG;
    }
    memcpy(old_path_buf, old_path, old_path_len);
    old_path_buf[old_path_len] = '\0';

    char new_path_buf[PATH_MAX];
    if (new_path_len >= PATH_MAX) {
        return ENAMETOOLONG;
    }
    memcpy(new_path_buf, new_path, new_path_len);
    new_path_buf[new_path_len] = '\0';

    if (symlinkat(old_path_buf, fd, new_path_buf) != 0) {
        return errno;
    }
    return 0;
}

int32_t path_rename(__WASM_MEMORY, int32_t old_fd, char *old_path,
                    wasm_size_t old_path_len, int32_t new_fd, char *new_path,
                    wasm_size_t new_path_len) {
    char old_path_buf[PATH_MAX];
    if (old_path_len >= PATH_MAX) {
        return ENAMETOOLONG;
    }
    memcpy(old_path_buf, old_path, old_path_len);
    old_path_buf[old_path_len] = '\0';

    char new_path_buf[PATH_MAX];
    if (new_path_len >= PATH_MAX) {
        return ENAMETOOLONG;
    }
    memcpy(new_path_buf, new_path, new_path_len);
    new_path_buf[new_path_len] = '\0';

    if (renameat(old_fd, old_path_buf, new_fd, new_path_buf) != 0) {
        return errno;
    }
    return 0;
}
