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

#if defined(PAW_OS_POSIX)

static void invoke_archiver(char *const argv[])
{
}

static void invoke_linker(char *const argv[])
{
    auto const pid = fork();
    if (pid < 0) {
        std::perror("unable to fork");
        std::abort();
    } else if (pid == 0) {
    try_again:
        if (execv(PAW_CLANG_PATH, argv) == -1) {
            if (errno == EINTR)
                goto try_again;
            std::perror("unable to execute \"clang\"");
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
    rewrite.reserve(1 // PAW_CLANG_PATH
            + objects_.size()
            + args_.size()
            + libs_.size()
            + 2   // -L$PAW_ROOT_DIR -lpaw_stdc
            + 2   // -L$PAW_GC_DIR -lgc
            + 1); // -o$path

    std::string const root_dir(PAW_ROOT_DIR);
    std::string const libgc_dir(PAW_GC_DIR);

    rewrite.push_back(PAW_CLANG_PATH);
    for (auto const &v: objects_) rewrite.push_back(v.data());
    for (auto const &v: args_) rewrite.push_back(v.data());
    for (auto const &v: libs_) rewrite.push_back("-l" + v);
    rewrite.push_back("-L" + libgc_dir + "/lib");
    rewrite.push_back("-lgc");
    rewrite.push_back("-L" + root_dir);
    rewrite.push_back("-lpaw_stdc");
    rewrite.push_back("-o" + path);

    std::vector<char const *> argv(rewrite.size() + 1);
    { // format arguments for "execv"
        auto itr = begin(argv);
        for (auto const &v: rewrite)
            *itr++ = v.c_str();
        *itr = nullptr;
    }

    invoke_linker(const_cast<char *const *>(argv.data()));
}

#else // !defined(PAW_OS_POSIX)

static void invoke_archiver(char *const argv[])
{
}

static void invoke_linker(char *const argv[])
{
}

void Linker::finalize(std::string path) &&
{
    PAW_UNREACHABLE();
}

#endif // !defined(PAW_OS_POSIX)


void Linker::add_arg(std::string value)
{
    LINKER_ASSERT(libs_.size() < MAX_ARGS, "too many linker arguments");
    args_.push_back(std::move(value));
}

void Linker::add_object(std::string path)
{
    LINKER_ASSERT(libs_.size() < MAX_OBJECTS, "too many objects to link");
    objects_.push_back(std::move(path));
}

void Linker::link_dylib(std::string path)
{
    LINKER_ASSERT(libs_.size() < MAX_LIBS, "too many libraries to link");
    libs_.push_back(std::move(path));
}

void Linker::link_staticlib(std::string path)
{
    LINKER_ASSERT(libs_.size() < MAX_LIBS, "too many libraries to link");
    libs_.push_back(std::move(path));
}

} // namespace paw::cg
