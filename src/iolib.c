#include "prefix.h"

#include "auxlib.h"
#include "call.h"
#include "lib.h"
#include "os.h"
#include <errno.h>
#include <stdio.h>

#if !defined(p_fseek)
#if defined(PAW_OS_POSIX)
#define p_fseek fseeko
#elif _MSC_VER >= 1400
#define p_fseek _fseeki64
#elif defined(__MINGW32__)
#define p_fseek fseeko64
#else
#define p_fseek fseek
#endif
#endif

#if !defined(p_ftell)
#if defined(PAW_OS_POSIX)
#define p_ftell ftello
#undef p_ftell
#define p_ftell ftell
#elif _MSC_VER >= 1400
#define p_ftell _ftelli64
#elif defined(__MINGW32__)
#define p_ftell ftello64
#else
#define p_ftell ftell
#endif
#endif

static FILE *get_handle(paw_Env *P, int index)
{
    FILE **pfile = paw_pointer(P, index);
    return *pfile;
}

static FILE **create_handle(paw_Env *P)
{
    return paw_create_foreign(P, sizeof(FILE *), 0);
}

static int io_open(paw_Env *P)
{
    pawL_check_argc(P, 2);
    const char *path = pawL_check_string(P, 1);
    const char *mode = pawL_check_string(P, 2);

    // Allocate space for the file handle
    FILE **pfile = create_handle(P);

    FILE *file = pawO_open(path, mode);
    if (file) {
        *pfile = file;
    } else {
        // FIXME: Not exception-worthy: return an error indicator of some sort (Lua returns
        //        2 things, nil and an error message.)
        pawO_system_error(P, errno);
    }
    return 1;
}

static int io_close(paw_Env *P)
{
    pawL_check_argc(P, 1);
    FILE *file = get_handle(P, 1);
    pawO_close(file);
    return 0;
}

static int io_flush(paw_Env *P)
{
    pawL_check_argc(P, 1);
    FILE *file = get_handle(P, 1);
    if (fflush(file)) {
        pawO_system_error(P, errno);
    }
    return 0;
}

static int io_read(paw_Env *P)
{
    pawL_check_argc(P, 2);
    pawL_check_type(P, 2, PAW_TINT);
    FILE *file = get_handle(P, 1);
    const paw_Int n = paw_int(P, 2);

    Buffer buf;
    pawL_init_buffer(P, &buf);
    pawL_buffer_resize(P, &buf, cast_size(n));
    pawO_read_exact(P, file, buf.data, buf.size);
    pawL_push_result(P, &buf);
    return 1;
}

static int io_write(paw_Env *P)
{
    pawL_check_argc(P, 2);
    const char *data = pawL_check_string(P, 2);
    FILE *file = get_handle(P, 1);
    pawO_write_all(P, file, data, paw_length(P, 2));
    return 0;
}

static int io_seek(paw_Env *P)
{
    pawL_check_argc(P, 3);
    const paw_Int offset = pawL_check_int(P, 2);
    const paw_Int origin = pawL_check_int(P, 3);
    FILE *file = get_handle(P, 1);
    if (p_fseek(file, offset, origin)) {
        pawO_system_error(P, errno);
    }
    return 0;
}

static int io_tell(paw_Env *P)
{
    pawL_check_argc(P, 1);
    FILE *file = get_handle(P, 1);
    paw_push_int(P, p_ftell(file));
    return 1;
}

static const pawL_Attr kIOLib[] = {
    {"open", io_open},
    {"close", io_close},
    {"flush", io_flush},
    {"read", io_read},
    {"write", io_write},
    {"seek", io_seek},
    {"tell", io_tell},
    {0},
};

void pawL_require_iolib(paw_Env *P)
{
    pawL_register_lib(P, IOLIB_NAME, 0, kIOLib);

    // Standard file streams
    *create_handle(P) = stdin;
    paw_set_attr(P, -2, "stdin");
    *create_handle(P) = stdout;
    paw_set_attr(P, -2, "stdout");
    *create_handle(P) = stderr;
    paw_set_attr(P, -2, "stderr");

    // 'origin' constants for io.seek()
    paw_push_int(P, SEEK_CUR);
    paw_set_attr(P, -2, "current");
    paw_push_int(P, SEEK_SET);
    paw_set_attr(P, -2, "begin");
    paw_push_int(P, SEEK_END);
    paw_set_attr(P, -2, "end");
}
