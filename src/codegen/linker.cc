// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "linker.h"
#include "util.h"

// TODO: probably should write separate linker abstractions for different linkers
#if defined(PAW_OS_POSIX)
# include <sys/wait.h>
# include <unistd.h>
#else
# error "Windows linker support not implemented"
#endif

#define LINKER_ASSERT(Expr_, Msg_) do { \
            if (!(Expr_)) { \
                std::fputs(Msg_ "", stderr); \
                std::abort(); \
            } \
        } while (0)

#define MAX_OBJECTS 1000
#define MAX_LIBS 1000
#define MAX_ARGS 1000

namespace paw::cg {

// Spawn a new process and execute the system linker with the specified arguments
static void invoke_linker_process(char const *linker, char *const argv[]);

static void link(std::string const &linker, std::vector<std::string> const &args)
{
    std::vector<char const *> rewrite(1 + args.size() + 1);
    { // format arguments for "execv"
        auto itr = begin(rewrite);
        *itr++ = linker.c_str();
        for (auto const &v: args)
            *itr++ = v.c_str();
        *itr = nullptr;
    }
    auto const *ptr = const_cast<char *const *>(rewrite.data());
    invoke_linker_process(linker.c_str(), ptr);
}

#if defined(PAW_OS_POSIX)

static void invoke_linker_process(char const *linker, char *const argv[])
{
    auto const pid = fork();
    if (pid < 0) {
        std::perror("unable to fork");
        std::abort();
    } else if (pid == 0) {
    try_again:
        if (execv(linker, argv) == -1) {
            if (errno == EINTR)
                goto try_again;
            std::perror("unable to execute linker");
            std::abort();
        }
    } else {
        int status;
        waitpid(pid, &status, 0);
        if (status < 0) {
            std::perror("unable to link");
            std::abort();
        }
    }
}

void Linker::finalize(std::string path) &&
{
    std::vector<std::string> rewrite;

    // NOTE: just an estimate
    rewrite.reserve(objects_.size()
            + args_.size()
            + dylibs_.size()
            + staticlibs_.size()
            + 1); // -o$path

    for (auto const &v: objects_) rewrite.push_back(v.data());
    for (auto const &v: args_) rewrite.push_back(v.data());

#if defined(PAW_OS_MACOS)
    // Apple clang seems to use the file extension to determine if a
    // library should be linked statically or dynamically.
    for (auto const &v: staticlibs_)
        rewrite.push_back("-l" + v);
    for (auto const &v: dylibs_)
        rewrite.push_back("-l" + v);
#else // defined(PAW_OS_MACOS)
    // On Linux, both gcc and clang support the "-B" option for
    // specifying linkage. Use "-Wl" to tell clang to forward the
    // rest of the argument string to the linker.
    for (auto const &v: staticlibs_) {
        rewrite.push_back("-Wl,-Bstatic");
        rewrite.push_back("-l" + v);
    }
    for (auto const &v: dylibs_) {
        rewrite.push_back("-Wl,-Bdynamic");
        rewrite.push_back("-l" + v);
    }
#endif // !defined(PAW_OS_MACOS)

    rewrite.push_back("-o" + path);
    link(PAW_CLANG_PATH, rewrite);
}

#else // !defined(PAW_OS_POSIX)

#error not supported

static void invoke_linker_process(char const *linker, char *const argv[])
{
    PAW_UNREACHABLE();
}

void Linker::finalize(std::string path) &&
{
    std::vector<std::string> rewrite;

    // NOTE: just an estimate
    rewrite.reserve(objects_.size()
            + args_.size()
            + dylibs_.size()
            + staticlibs_.size()
            + 1); // -o$path

    rewrite.push_back("-o" + path);
    link(PAW_CLANG_PATH, rewrite);
}

#endif // !defined(PAW_OS_POSIX)


void Linker::add_arg(std::string value)
{
    LINKER_ASSERT(args_.size() < MAX_ARGS, "too many linker arguments");
    args_.push_back(std::move(value));
}

void Linker::add_object(std::string path)
{
    LINKER_ASSERT(objects_.size() < MAX_OBJECTS, "too many objects to link");
    objects_.push_back(std::move(path));
}

void Linker::link_dylib(std::string path)
{
    LINKER_ASSERT(staticlibs_.size() < MAX_LIBS - dylibs_.size(),
            "too many libraries to link");
    dylibs_.push_back(std::move(path));
}

void Linker::link_staticlib(std::string path)
{
    LINKER_ASSERT(staticlibs_.size() < MAX_LIBS - dylibs_.size(),
            "too many libraries to link");
    staticlibs_.push_back(std::move(path));
}

} // namespace paw::cg
