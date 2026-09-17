// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "paw.h"

#define NANOS 1000000000

#if defined(__APPLE__) || defined(__linux__)
# include <errno.h>
# include <time.h>

void paw_os_sleep(paw_Uint64 nanos)
{
    struct timespec rem;
    struct timespec dur = {
        .tv_sec = nanos / NANOS,
        .tv_nsec = nanos % NANOS,
    };
    // There are 3 possible errors that `nanosleep` can encounter: one of the pointer
    // arguments is invalid (EFAULT), `dur.tv_nsec` is less than 0 or greater than or
    // equal to `NANOS` (EINVAL), or the system call is interrupted (EINTR). Only the
    // EINTR case needs to be handled here.
    while (nanosleep(&dur, &rem) != 0) {
        paw_assert(errno == EINTR);
        dur.tv_sec = rem.tv_sec;
        dur.tv_nsec = rem.tv_nsec;
    }
}

#elif defined(_WIN32)
# include <Windows.h>

void paw_os_sleep(paw_Uint64 nanos)
{
    Sleep(nanos / 1000);
}

#else
# error "unrecognized target platform"
#endif
