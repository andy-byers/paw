// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_CODEGEN_LINKER_H
#define PAW_CODEGEN_LINKER_H

#include "core.h"
#include <string>
#include <vector>

namespace paw::cg {

// Linker abstraction
// Uses clang as the linker driver.
class Linker final {
public:
    explicit Linker(paw_Env *P)
        : P(P)
    {
    }

    ~Linker() = default;
    Linker(Linker &&) = default;
    Linker &operator=(Linker &&) = default;

    Linker(Linker const &) = delete;
    Linker &operator=(Linker const &) = delete;

    paw_Env *env() const { return P; }

    void add_arg(std::string value);
    void add_object(std::string path);
    void link_dylib(std::string path);
    void link_staticlib(std::string path);
    void finalize(std::string path) &&;

    Linker with_arg(std::string value) &&
    {
        add_arg(std::move(value));
        return std::move(*this);
    }

    Linker with_object(std::string path) &&
    {
        add_object(std::move(path));
        return std::move(*this);
    }

    Linker with_dylib(std::string path) &&
    {
        link_dylib(std::move(path));
        return std::move(*this);
    }

    Linker with_staticlib(std::string path) &&
    {
        link_staticlib(std::move(path));
        return std::move(*this);
    }

private:
    std::vector<std::string> objects_;
    std::vector<std::string> args_;
    std::vector<std::string> libs_;
    paw_Env *P;
};

} // namespace paw::cg

#endif // PAW_CODEGEN_LINKER_H
