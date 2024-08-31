// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_CONFIG_H
#define PAW_CONFIG_H

#include <stdint.h>

#define PAW_INT_WIDTH (sizeof(paw_Int) * 8)
#define PAW_INT_MAX INT64_MAX
#define PAW_INT_MIN INT64_MIN
#define PAW_SIZE_MAX (sizeof(size_t) < sizeof(paw_Int) \
        ? SIZE_MAX : (size_t)PAW_INT_MAX)
#define PAW_CAST_INT(x) ((paw_Int)(x))
#define PAW_INT_C(x) INT64_C(x)

#ifndef PAW_ALIGN
# define PAW_ALIGN 8
#endif

#ifndef PAW_NAME_MAX
# define PAW_NAME_MAX 128
#endif

#ifndef PAW_STACK_MAX
# define PAW_STACK_MAX 1000000
#endif

#ifndef PAW_HEAP_DEFAULT
# define PAW_HEAP_DEFAULT ((size_t)33554432)
#endif

#if defined(__APPLE__)
# define PAW_OS_MACOS
# define PAW_OS_POSIX
#elif defined(__linux__)
# define PAW_OS_LINUX
# define PAW_OS_POSIX
#elif defined(_WIN32)
# define PAW_OS_WINDOWS
#endif

#if defined(__GNUC__) || defined(__clang__)
# define PAW_NODISCARD __attribute__((warn_unused_result))
# define PAW_UNREACHABLE __builtin_unreachable
#elif defined(_MSC_VER)
# define PAW_NODISCARD _Check_return_
# define PAW_UNREACHABLE paw_unreachable_
_Noreturn static inline void paw_unreachable_(void) 
{
    __assume(0);
}
#else
# define PAW_NODISCARD
# define PAW_UNREACHABLE paw_unreachable_
_Noreturn static inline void paw_unreachable_(void) {}
#endif

#if !defined(PAW_LIKELY)
# if defined(__GNUC__) || defined(__clang__)
#  define P_LIKELY(x) (__builtin_expect((x) != 0, 1))
#  define P_UNLIKELY(x) (__builtin_expect((x) != 0, 0))
# else
#  define PAW_LIKELY(x) (x)
#  define PAW_UNLIKELY(x) (x)
# endif
#endif

#endif // PAW_CONFIG_H
