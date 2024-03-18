// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#include "bigint.h"
#include "aux.h"
#include "error.h"
#include "gc.h"
#include "mem.h"
#include "rt.h"

#define swap(T, x, y) \
    {                 \
        T t = (x);    \
        (x) = (y);    \
        (y) = t;      \
    }

static BigInt *negate(BigInt *bi)
{
    bi->neg = !bi->neg;
    return bi;
}

static void bi_ensure(paw_Env *P, BigInt *bi, int size)
{
    paw_assert(size > 0);
    if (size >= bi->alloc) {
        // First power-of-2 greater than 'size'
        int alloc = 1;
        while (alloc <= size) {
            alloc *= 2;
        }
        pawM_resize(P, bi->buf, cast_size(bi->alloc), cast_size(alloc));
        memset(bi->buf + bi->size, 0, cast_size(size - bi->size) * sizeof(BiDigit));
        bi->alloc = alloc;
    }
}

static void copy_digits(BiDigit *dst, const BiDigit *src, int n)
{
    memcpy(dst, src, cast_size(n) * sizeof(src[0]));
}

static BigInt *bi_alloc(paw_Env *P, StackPtr sp, int n)
{
    BigInt *bi = pawB_new(P);
    pawV_set_bigint(sp, bi); // Anchor
    bi_ensure(P, bi, n);
    bi->size = n;
    return bi;
}

BigInt *pawB_copy(paw_Env *P, StackPtr sp, const BigInt *bi, int extra)
{
    BigInt *bi2 = bi_alloc(P, sp, bi->size + extra);
    copy_digits(bi2->buf, bi->buf, bi->size);
    bi2->size = bi->size;
    bi2->neg = bi->neg;
    return bi2;
}

static void bi_trim(BigInt *bi)
{
    while (bi->size > 0) {
        if (bi->buf[bi->size - 1]) {
            break;
        }
        --bi->size;
    }
}

static int cmp_raw_digits(const BiDigit *x, int nx, const BiDigit *y, int ny)
{
    if (nx < ny) {
        return -1;
    } else if (nx > ny) {
        return 1;
    }
    // Compare the digits, most-significant first.
    for (int i = nx - 1; i >= 0; --i) {
        if (x[i] < y[i]) {
            return -1;
        } else if (x[i] > y[i]) {
            return 1;
        }
    }
    return 0;
}

static int unpack_int(paw_Int i, BiDigit *digits)
{
    if (i == 0) {
        return 0;
    } else if (i < 0) {
        paw_assert(i != PAW_INT_MIN); // TODO
        i = -i;
    }
    BiDigit *p = digits;
    do {
        *p++ = i % BI_BASE;
        i /= BI_BASE;
    } while (i);
    return p - digits;
}

// Unpack an integral value into a BigInt
// 'buf' is assumed to be large enough to fit a paw_Int. If 'v' is a BigInt already,
// its fields are just copied.
static int unpack_aux(const Value v, BiDigit *buf, BigInt *out)
{
    paw_assert(!pawV_is_float(v));
    paw_assert(!pawV_is_bool(v));

    if (pawV_is_int(v)) {
        paw_Int i = pawV_get_int(v);
        out->buf = buf;
        out->size = unpack_int(i, buf);
        out->neg = i < 0;
    } else if (pawV_is_bigint(v)) {
        BigInt *src = pawV_get_bigint(v);
        out->buf = src->buf;
        out->size = src->size;
        out->neg = src->neg;
    } else {
        return -1;
    }
    return 0;
}

// Convenience function for preparing to run a binary operator
static void unpack2(paw_Env *P, Op op, //
                    const Value x, BiDigit *xbuf, BigInt *xout,
                    const Value y, BiDigit *ybuf, BigInt *yout)
{
    if (unpack_aux(x, xbuf, xout) || unpack_aux(y, ybuf, yout)) {
        pawE_type2(P, pawR_opcode_name(op));
    }
}

static int cmp_bi_bi(const BigInt *x, const BigInt *y)
{
    if (bi_zero(x) && bi_zero(y)) {
        return 0;
    } else if (x->neg != y->neg) {
        return x->neg ? -1 : 1;
    }
    // Compare magnitudes
    const int cmp = cmp_raw_digits(x->buf, x->size, y->buf, y->size);
    return x->neg ? -cmp : cmp;
}

static int cmp_bi_i(const BigInt *x, paw_Int y)
{
    if (bi_zero(x) && y == 0) {
        return 0;
    } else if (x->neg != (y < 0)) {
        return x->neg ? -1 : 1;
    }
    BiDigit buf[UNPACKED_INT_SIZE];
    const int ysize = unpack_int(y, buf);
    const int cmp = cmp_raw_digits(x->buf, x->size, buf, ysize);
    return x->neg ? -cmp : cmp;
}

#define gen_cmp(name, op)                                 \
    static paw_Bool bi_##name(BigInt *lhs, const Value v) \
    {                                                     \
        if (pawV_is_int(v)) {                             \
            const paw_Int rhs = pawV_get_int(v);          \
            return cmp_bi_i(lhs, rhs) op 0;               \
        } else {                                          \
            paw_assert(pawV_is_bigint(v));                \
            const BigInt *rhs = pawV_get_bigint(v);       \
            return cmp_bi_bi(lhs, rhs) op 0;              \
        }                                                 \
    }
gen_cmp(lt, <)
    gen_cmp(le, <=)
        gen_cmp(eq, ==)

            static BigInt *addm(paw_Env *P, StackPtr sp, const BigInt *x, const BigInt *y)
{
    const int min = paw_min(x->size, y->size);
    const int max = paw_max(x->size, y->size);
    BigInt *z = bi_alloc(P, sp, max + 1);
    if (x->size < y->size) {
        swap(const BigInt *, x, y);
    }

    int i = 0;
    int c = 0;
    for (; i < min; ++i) {
        const int digit = x->buf[i] + y->buf[i] + c;
        z->buf[i] = digit;
        c = digit >> BI_BITS;
    }

    for (; i < max; ++i) {
        const int digit = x->buf[i] + c;
        z->buf[i] = digit;
        c = digit >> BI_BITS;
    }
    if (c) {
        // Add the carry
        z->buf[i++] = c;
    }
    z->size = i;
    return z;
}

static BigInt *subm(paw_Env *P, StackPtr sp, const BigInt *x, const BigInt *y)
{
    const int min = paw_min(x->size, y->size);
    const int max = paw_max(x->size, y->size);
    BigInt *z = bi_alloc(P, sp, max + 1);

    paw_Bool swap = PAW_BFALSE;
    if (cmp_raw_digits(x->buf, x->size, y->buf, y->size) < 0) {
        swap(const BigInt *, x, y); // |x| >= |y| must be true
        swap = PAW_BTRUE;
    }

    int i = 0;
    int c = 0;
    for (; i < min; ++i) {
        const int digit = x->buf[i] - y->buf[i] + c;
        z->buf[i] = digit;
        c = digit < 0 ? -1 : 0;
    }

    for (; i < max; ++i) {
        const int digit = x->buf[i] + c;
        z->buf[i] = digit;
        c = digit < 0 ? -1 : 0;
    }
    if (c) {
        // Add the carry
        z->buf[i++] = c;
    }
    z->size = i;
    if (swap) {
        negate(z);
    }
    bi_trim(z);
    return z;
}

// Compute a multi-precision integer product
// Rearrangement of 'schoolbook' division. See HAC 14.2.3
static BigInt *mulm(paw_Env *P, StackPtr sp, const BiDigit *x, int nx, const BiDigit *y, int ny)
{
    BigInt *bi = bi_alloc(P, sp, nx + ny);
    BiDigit *ofs = bi->buf;
    for (int i = 0; i < ny; ++i, ++ofs) {
        BiDigit *w = ofs;
        int c = 0;

        for (int j = 0; j < nx; ++j, ++w) {
            const int uv = *w + x[j] * y[i] + c;
            *w = uv & BI_MASK; // w = v
            c = uv >> BI_BITS; // c = u
        }
        if (c) {
            *w++ = c;
        }
        bi->size = w - bi->buf;
    }
    return bi;
}

// From micropython multi-precision integer module
static void divmod_aux(BiDigit *quo_dig, int *quo_len, BiDigit *num_dig, int *num_len, const BiDigit *den_dig, int den_len)
{
    BiDigit *orig_num_dig = num_dig;
    BiDigit *orig_quo_dig = quo_dig;
    BiDigit norm_shift = 0;
    int lead_den_digit;

    {
        int cmp = cmp_raw_digits(num_dig, *num_len, den_dig, den_len);
        if (cmp == 0) {
            *num_len = 0;
            quo_dig[0] = 1;
            *quo_len = 1;
            return;
        } else if (cmp < 0) {
            *quo_len = 0;
            return;
        }
    }

    {
        BiDigit d = den_dig[den_len - 1];
        while ((d & BI_MSB) == 0) {
            d <<= 1;
            ++norm_shift;
        }
    }

    num_dig[*num_len] = 0;
    ++(*num_len);
    for (BiDigit *num = num_dig, carry = 0; num < num_dig + *num_len; ++num) {
        BiDigit n = *num;
        *num = ((n << norm_shift) | carry) & BI_MASK;
        carry = (int)n >> (BI_BITS - norm_shift);
    }

    lead_den_digit = (int)den_dig[den_len - 1] << norm_shift;
    if (den_len >= 2) {
        lead_den_digit |= (int)den_dig[den_len - 2] >> (BI_BITS - norm_shift);
    }

    num_dig += *num_len - 1;
    *quo_len = *num_len - den_len;
    quo_dig += *quo_len - 1;
    while (*num_len > den_len) {
        int quo = ((int)*num_dig << BI_BITS) | num_dig[-1];
        quo /= lead_den_digit;

        const BiDigit *d = den_dig;
        int d_norm = 0;
        int borrow = 0;
        for (BiDigit *n = num_dig - den_len; n < num_dig; ++n, ++d) {
            d_norm = ((int)*d << norm_shift) | (d_norm >> BI_BITS);
            int x = (int)quo * (d_norm & BI_MASK);
            int low_digs = (borrow & BI_MASK) + *n - (x & BI_MASK);
            *n = low_digs & BI_MASK;
            borrow = (borrow >> BI_BITS) - (x >> BI_BITS) + (low_digs >> BI_BITS);
        }

        borrow += *num_dig;
        for (; borrow != 0; --quo) {
            d = den_dig;
            d_norm = 0;
            int carry = 0;
            for (BiDigit *n = num_dig - den_len; n < num_dig; ++n, ++d) {
                d_norm = ((int)*d << norm_shift) | (d_norm >> BI_BITS);
                carry += (int)*n + (d_norm & BI_MASK);
                *n = carry & BI_MASK;
                carry >>= BI_BITS;
            }
            borrow += carry;
        }

        *quo_dig = quo & BI_MASK;
        --*num_len;
        --quo_dig;
        --num_dig;
    }

    for (BiDigit *num = orig_num_dig + *num_len - 1, carry = 0; num >= orig_num_dig; --num) {
        BiDigit n = *num;
        *num = ((n >> norm_shift) | carry) & BI_MASK;
        carry = (int)n << (BI_BITS - norm_shift);
    }
    while (*quo_len > 0 && orig_quo_dig[*quo_len - 1] == 0) {
        --(*quo_len);
    }
    while (*num_len > 0 && orig_num_dig[*num_len - 1] == 0) {
        --(*num_len);
    }
}

// Given 'x = qy + r', compute 'q' and 'r'
// See HAC 14.20.
static void bi_divmod(paw_Env *P, StackPtr sp, const BigInt *x, const BigInt *y)
{
    if (bi_zero(y)) {
        pawE_error(P, PAW_ERUNTIME, "divide by 0");
    }
    StackPtr rp = pawC_stkinc(P, 1);
    sp = rp - 1; // Instead of saving/loading position
    BigInt *r = pawB_copy(P, rp, x, 1);
    r->neg = x->neg != y->neg;
    BigInt *q = bi_alloc(P, sp, x->size + 1);
    q->neg = r->neg;
    int cmp = cmp_raw_digits(x->buf, x->size, y->buf, y->size);
    if (cmp == 0) {
        // If x == y, then q == 1 and r == 0.
        r->size = 0;
        q->size = 1;
        q->buf[0] = 1;
        return;
    } else if (cmp < 0) {
        // If x < y, then q == 1 and r == x.
        q->size = 0;
        return;
    }
    divmod_aux(q->buf, &q->size, r->buf, &r->size, y->buf, y->size);
}

static void bi_div(StackPtr sp, BigInt *x, BigInt *y)
{
    const paw_Float f = pawB_get_float(x);
    const paw_Float g = pawB_get_float(y);
    pawV_set_float(sp, f / g);
}

static void bi_idiv(paw_Env *P, StackPtr sp, BigInt *x, BigInt *y)
{
    bi_divmod(P, sp, x, y);
    paw_pop(P, 1); // Pop 'r'
}

static void bi_mod(paw_Env *P, StackPtr sp, BigInt *x, BigInt *y)
{
    bi_divmod(P, sp, x, y);
    paw_replace(P, -2); // Replace 'q' with 'r'
}

// Compute 'x + y', where both 'x' and 'y' are big integers
// Transformations:
//     ( x) + ( y) -->  addm(x, y)
//     ( x) + (-y) -->  subm(x, y)
//     (-x) + ( y) -->  subm(y, x)
//     (-x) + (-y) --> -addm(x, y)
static void bi_add(paw_Env *P, StackPtr sp, BigInt *x, BigInt *y)
{
    if (x->neg) {
        if (y->neg) { // x < 0 && y < 0
            negate(addm(P, sp, x, y));
        } else { // x < 0 && y >= 0
            subm(P, sp, y, x);
        }
    } else if (y->neg) { // x >= 0 && y < 0
        subm(P, sp, x, y);
    } else { // x >= 0 && y >= 0
        addm(P, sp, x, y);
    }
}

// Compute 'x - y', where both 'x' and 'y' are big integers
// Transformations:
//     ( x) - ( y) -->  subm(x, y)
//     ( x) - (-y) -->  addm(x, y)
//     (-x) - ( y) --> -addm(x, y)
//     (-x) - (-y) -->  subm(y, x)
static void bi_sub(paw_Env *P, StackPtr sp, BigInt *x, BigInt *y)
{
    if (x->neg) {
        if (y->neg) { // x < 0 && y < 0
            subm(P, sp, y, x);
        } else { // x < 0 && y >= 0
            negate(addm(P, sp, x, y));
        }
    } else if (y->neg) { // x >= 0 && y < 0
        addm(P, sp, x, y);
    } else { // x >= 0 && y >= 0
        subm(P, sp, x, y);
    }
}

static void bi_mul(paw_Env *P, StackPtr sp, BigInt *x, BigInt *y)
{
    if (bi_zero(x) || bi_zero(y)) {
        pawV_set_int(sp, 0);
        return;
    }
    BigInt *bi = mulm(P, sp, x->buf, x->size, y->buf, y->size);
    bi->neg = x->neg != y->neg;
}

static void bi_band(paw_Env *P, StackPtr sp, BigInt *x, BigInt *y)
{
    if (x->size < y->size) {
        swap(BigInt *, x, y);
    }
    int xcarry = x->neg;
    int ycarry = y->neg;
    int zcarry = xcarry == ycarry ? x->neg : 0;
    int zmask = zcarry ? BI_MASK : 0;
    int xmask = xcarry ? BI_MASK : 0;
    int ymask = ycarry ? BI_MASK : 0;
    BigInt *z = bi_alloc(P, sp, x->size + 1);
    for (int i = 0; i < x->size; ++i) {
        xcarry += x->buf[i] ^ xmask;
        ycarry += y->size > i ? y->buf[i] ^ ymask : ymask;
        zcarry += ((xcarry & ycarry) ^ zmask) & BI_MASK;
        z->buf[i] = zcarry & BI_MASK;
        xcarry >>= BI_BITS;
        ycarry >>= BI_BITS;
        zcarry >>= BI_BITS;
    }
    if (zcarry) {
        z->buf[x->size] = zcarry;
    }
    z->neg = x->neg & y->neg;
    bi_trim(z);
}

static void bi_bor(paw_Env *P, StackPtr sp, BigInt *x, BigInt *y)
{
    if (x->size < y->size) {
        swap(BigInt *, x, y);
    }
    int xcarry = x->neg;
    int ycarry = y->neg;
    int zcarry = xcarry || ycarry;
    int zmask = zcarry ? BI_MASK : 0;
    int xmask = xcarry ? BI_MASK : 0;
    int ymask = ycarry ? BI_MASK : 0;
    BigInt *z = bi_alloc(P, sp, x->size);
    for (int i = 0; i < x->size; ++i) {
        xcarry += x->buf[i] ^ xmask;
        ycarry += y->size > i ? y->buf[i] ^ ymask : ymask;
        zcarry += ((xcarry | ycarry) ^ zmask) & BI_MASK;
        z->buf[i] = zcarry & BI_MASK;
        xcarry >>= BI_BITS;
        ycarry >>= BI_BITS;
        zcarry >>= BI_BITS;
    }
    paw_assert(!zcarry);
    z->neg = x->neg | y->neg;
    bi_trim(z);
}

static void bi_bxor(paw_Env *P, StackPtr sp, BigInt *x, BigInt *y)
{
    if (x->size < y->size) {
        swap(BigInt *, x, y);
    }
    int zcarry = 0;
    int xcarry = x->neg;
    int ycarry = y->neg;
    BigInt *z = bi_alloc(P, sp, x->size + 1);
    for (int i = 0; i < x->size; ++i) {
        xcarry += x->buf[i] + BI_MASK;
        ycarry += y->size > i ? y->buf[i] + BI_MASK : BI_MASK;
        zcarry += (xcarry ^ ycarry) & BI_MASK;
        z->buf[i] = zcarry & BI_MASK;
        xcarry >>= BI_BITS;
        ycarry >>= BI_BITS;
        zcarry >>= BI_BITS;
    }
    if (zcarry) {
        z->buf[x->size] = zcarry;
    }
    z->neg = x->neg ^ y->neg;
    bi_trim(z);
}

static void bi_shl(paw_Env *P, StackPtr sp, BigInt *bi, paw_Int n)
{
    const int nwhole = (n + BI_BITS - 1) / BI_BITS;
    int nfract = n % BI_BITS;
    if (nfract == 0) {
        nfract = BI_BITS;
    }

    int nz = bi->size;
    BigInt *out = pawB_copy(P, sp, bi, nwhole);
    BiDigit *z = out->buf + nwhole + nz - 1;
    BiDigit *x = bi->buf + nz - 1;

    uint64_t d = 0;
    for (int i = 0; i < nz; ++i, --x, --z) {
        d |= *x;
        *z = (d >> (BI_BITS - nfract)) & BI_MASK;
        d <<= BI_BITS;
    }

    *z = (d >> (BI_BITS - nfract)) & BI_MASK;
    z -= nwhole - 1;
    memset(z, 0, cast_size(nwhole - 1) * sizeof(z[0]));

    nz += nwhole;
    while (nz && !z[nz - 1]) {
        --nz;
    }
    out->size = nz;
}

static void bi_shr(paw_Env *P, StackPtr sp, BigInt *bi, paw_Int n)
{
    const int nwhole = n / BI_BITS;
    const int nfract = n % BI_BITS;

    if (nwhole >= bi->size) {
        pawV_set_int(sp, bi->neg ? -1 : 0);
        return;
    }
    BigInt *out = bi_alloc(P, sp, bi->size);
    out->neg = bi->neg; // Propagate sign
    const BiDigit *x = bi->buf + nwhole;
    int nz = bi->size - nwhole;
    BiDigit *z = out->buf;

    for (int i = 0; i < nz; ++i, ++z) {
        int d = x[i];
        if (i < nz - 1) {
            d |= (int)x[i + 1] << BI_BITS;
        }
        d >>= nfract;
        *z = d & BI_MASK;
    }
    if (!z[-1]) {
        --nz;
    }
    out->size = nz;
}

static size_t finish_string(char *str, int len, const char *prefix, paw_Bool negative)
{
    // 'str' must have room for 2 more chars at least.
    char *ptr = str + len;

    // Prefix must be '0*' (where '*' indicates the base) or the empty string. Add it
    // to the output in reverse.
    const int np = prefix[0] ? 2 : 0;
    for (int i = 0; i < np; ++i) {
        *ptr++ = prefix[np - i - 1];
    }
    len += np;

    if (negative) {
        *ptr++ = '-';
        ++len;
    }

    // Reverse the whole string.
    char *x = str;
    char *y = str + len - 1;
    for (; x < y; ++x, --y) {
        const char t = *x;
        *x = *y;
        *y = t;
    }
    return cast_size(len);
}

void pawB_str(paw_Env *P, const BigInt *bi, paw_Bool caps, const char *prefix, int base)
{
    paw_assert(2 <= base && base <= 32);
    const int char_offset = caps ? 'A' : 'a';

    if (bi_zero(bi)) {
        // 'zero' needs space for '0*0', where '*' is a single char
        // representing the base.
        char zero[3] = {'0'};
        const size_t n = finish_string(zero, 1, prefix, PAW_BFALSE);
        // Push the string.
        StackPtr sp = pawC_stkinc(P, 1);
        String *s = pawS_new_nstr(P, zero, n);
        pawV_set_string(sp, s);
        return;
    }
    Buffer print;
    pawL_init_buffer(P, &print);
    const int n = bi->size;
    // We cannot return or call any routine that throws until 'digits'
    // is freed.
    BiDigit *digits = pawM_new_vec(P, n, BiDigit);
    memcpy(digits, bi->buf, cast_size(n) * sizeof(digits[0]));

    // Compute the divmod. We have to go over the digits repeatedly until
    // they are all zero, since the BI_BASE may not be divisible by the
    // target base. If it was, the computation could be simplified.
    paw_Bool zero;
    do {
        BiDigit *p = digits + n;
        int c = 0;

        for (int i = 0; i < n; ++i) {
            c = c << BI_BITS | *--p;
            *p = c / base;
            c %= base;
        }
        // Convert to character
        c += c > 9 ? char_offset : '0';
        pawL_add_char(P, &print, c);

        zero = PAW_BTRUE;
        for (int i = 0; i < n; ++i) {
            if (digits[i]) {
                zero = PAW_BFALSE;
                break;
            }
        }
    } while (!zero);
    pawM_free_vec(P, digits, n);

    print.size = finish_string(print.data, print.size,
                               prefix, bi->neg);
    pawL_push_result(P, &print);
}

void pawB_from_float(paw_Env *P, paw_Float f)
{
    // TODO: Check for inf, NaN
    StackPtr sp = pawC_stkinc(P, 1);
    pawB_from_int(P, sp, (paw_Int)f);
}

paw_Int pawB_get_int64(const BigInt *bi, paw_Bool *lossless)
{
    paw_Int value = 0;
    *lossless = PAW_BTRUE;
    for (int i = bi->size - 1; i >= 0; --i) {
        const int v = bi->buf[i];
        if (value > (PAW_INT_MAX - v) / BI_BASE) {
#define LOW_MASK ((paw_int_c(1) << (PAW_INT_WIDTH - BI_BITS)) - 1)
            // Clear the high bits so 'value' doesn't overflow.
            value &= LOW_MASK;
            *lossless = PAW_BFALSE;
        }
        value = value * BI_BASE + v;
    }
    return bi->neg ? -value : value;
}

paw_Int pawB_get_int(const BigInt *bi)
{
    paw_Int value = 0;
    for (int i = bi->size - 1; i >= 0; --i) {
        const int v = bi->buf[i];
        // Caller must make sure the magnitude isn't too large. Assertion adds 1
        // if 'bi' is negative to allow VINT_MIN to be parsed.
        paw_assert(value <= (VINT_MAX - v) / BI_BASE + !!bi->neg);
        value = value * BI_BASE + v;
    }
    return bi->neg ? -value : value;
}

paw_Float pawB_get_float(const BigInt *bi)
{
    paw_Float value = 0.0;
    for (int i = 0; i < bi->size; ++i) {
        const int v = bi->buf[bi->size - i - 1];
        value = value * BI_BASE + v;
    }
    return bi->neg ? -value : value;
}

BigInt *pawB_from_int(paw_Env *P, StackPtr sp, paw_Int i)
{
    BiDigit buf[UNPACKED_INT_SIZE];
    const int n = unpack_int(i, buf);
    BigInt *bi = bi_alloc(P, sp, n);

    memcpy(bi->buf, buf, cast_size(n) * sizeof(buf[0]));
    bi->neg = i < 0;
    return bi;
}

BigInt *pawB_new(paw_Env *P)
{
    BigInt *bi = pawM_new(P, BigInt);
    pawG_add_object(P, cast_object(bi), VBIGINT);
    return bi;
}

void pawB_free(paw_Env *P, BigInt *bi)
{
    pawM_free_vec(P, bi->buf, bi->alloc);
    pawM_free(P, bi);
}

// Compute 'y = ~x'
// Equivalent to 'y = -x - 1'.
static void bi_bnot(paw_Env *P, StackPtr sp, BigInt *x)
{
    if (bi_zero(x)) {
        pawV_set_int(sp, -1);
        return;
    }

    BiDigit k = 1;
    BigInt one = {.buf = &k, .size = 1};
    if (x->neg) {
        subm(P, sp, x, &one);
    } else {
        addm(P, sp, x, &one);
    }
    BigInt *bi = pawV_get_bigint(*sp);
    bi->neg = !x->neg;
}

void pawB_unop(paw_Env *P, Op op, Value x)
{
    paw_assert(pawV_is_bigint(x));
    BigInt *bi = pawV_get_bigint(x);
    StackPtr sp = pawC_stkinc(P, 1);
    switch (op) {
        case OP_NEG:
            negate(pawB_copy(P, sp, bi, 0));
            break;
        case OP_NOT:
            pawV_set_bool(sp, bi_zero(bi));
            break;
        case OP_BNOT:
            bi_bnot(P, sp, bi);
            break;
        default:
            pawE_type(P, pawR_opcode_name(op));
    }
}

static paw_Bool is_negative(const Value v)
{
    if (pawV_is_int(v)) {
        return pawV_get_int(v) < 0;
    } else if (pawV_is_bigint(v)) {
        return pawV_get_bigint(v)->neg;
    } else if (pawV_is_float(v)) {
        return pawV_get_float(v) < 0;
    } else {
        return PAW_BFALSE;
    }
}

static paw_Bool fits_in_smallint(Value v)
{
    const BigInt *bi = pawV_get_bigint(v);
    if (bi->neg) {
        return cmp_bi_i(bi, VINT_MIN) >= 0;
    } else {
        return cmp_bi_i(bi, VINT_MAX) <= 0;
    }
}

// Convert the results of a bigint operation back to VINT, if possible
static void bi_finish(Value *pv)
{
    if (pawV_is_bigint(*pv) && fits_in_smallint(*pv)) {
        BigInt *bi = pawV_get_bigint(*pv);
        pawV_set_int(pv, pawB_get_int(bi));
    }
}

// This function only handles situations where either 'lhs' or 'rhs' is a BigInt,
// or they are both paw_Int and the operation would overflow. If either is a paw_Float,
// then float operations should be used. Caller must convert paw_Bool to paw_Int
// beforehand.
void pawB_arith(paw_Env *P, Op op, Value lhs, Value rhs)
{
    paw_assert(!pawV_is_float(lhs) && !pawV_is_float(rhs));
    paw_assert(!pawV_is_bool(lhs) && !pawV_is_bool(rhs));

    BigInt x = {0};
    BigInt y = {0};
    BiDigit xbuf[UNPACKED_INT_SIZE];
    BiDigit ybuf[UNPACKED_INT_SIZE];
    unpack2(P, op, // unpack into BigInt
            lhs, xbuf, &x,
            rhs, ybuf, &y);

    StackPtr sp = pawC_stkinc(P, 1);
    switch (op) {
        case OP_ADD:
            bi_add(P, sp, &x, &y);
            break;
        case OP_SUB:
            bi_sub(P, sp, &x, &y);
            break;
        case OP_MUL:
            bi_mul(P, sp, &x, &y);
            break;
        case OP_DIV:
            bi_div(sp, &x, &y);
            return; // result is VFLOAT
        case OP_IDIV:
            bi_idiv(P, sp, &x, &y);
            break;
        case OP_MOD:
            bi_mod(P, sp, &x, &y);
            break;
        default:
            pawE_type2(P, pawR_opcode_name(op));
    }
    bi_finish(sp);
}

void pawB_bitwise(paw_Env *P, Op op, Value lhs, Value rhs)
{
    paw_assert(!pawV_is_float(lhs) && !pawV_is_float(rhs));
    paw_assert(!pawV_is_bool(lhs) && !pawV_is_bool(rhs));

    BigInt x = {0};
    BigInt y = {0};
    BiDigit xbuf[UNPACKED_INT_SIZE];
    BiDigit ybuf[UNPACKED_INT_SIZE];
    unpack2(P, op, // unpack into BigInt
            lhs, xbuf, &x,
            rhs, ybuf, &y);

    StackPtr sp = pawC_stkinc(P, 1);
    switch (op) {
        case OP_BAND:
            bi_band(P, sp, &x, &y);
            break;
        case OP_BOR:
            bi_bor(P, sp, &x, &y);
            break;
        case OP_BXOR:
            bi_bxor(P, sp, &x, &y);
            break;
        case OP_SHL: {
            if (is_negative(rhs)) {
                pawE_error(P, PAW_ERANGE, "negative shift count");
            } else if (pawV_is_bigint(rhs)) {
                // Definitely OOM: this would require an enormous amount of memory.
                pawE_error(P, PAW_ERANGE, "shift count too large");
            }
            const paw_Int n = pawV_get_int(rhs);
            bi_shl(P, sp, &x, n);
            break;
        }
        case OP_SHR: {
            if (is_negative(rhs)) {
                pawE_error(P, PAW_ERANGE, "negative shift count");
            } else if (pawV_is_bigint(rhs)) {
                // just propagate the sign
                pawV_set_int(sp, is_negative(lhs) ? -1 : 0);
                break;
            }
            const paw_Int n = pawV_get_int(rhs);
            bi_shr(P, sp, &x, n);
            break;
        }
        default:
            pawE_type2(P, pawR_opcode_name(op));
    }
    bi_finish(sp);
}

paw_Bool pawB_cmp(Op op, Value lhs, Value rhs)
{
    paw_assert(pawV_is_bigint(lhs) || pawV_is_bigint(rhs));
    paw_assert(!pawV_is_float(lhs) && !pawV_is_float(rhs));
    paw_assert(!pawV_is_bool(lhs) && !pawV_is_bool(rhs));

    switch (op) {
        case OP_LT:
            if (pawV_is_bigint(lhs)) {
                return bi_lt(pawV_get_bigint(lhs), rhs);
            } else {
                swap(Value, lhs, rhs);
                return !bi_le(pawV_get_bigint(lhs), rhs);
            }
            break;
        case OP_LE:
            if (pawV_is_bigint(lhs)) {
                return bi_le(pawV_get_bigint(lhs), rhs);
            } else {
                swap(Value, lhs, rhs);
                return !bi_lt(pawV_get_bigint(lhs), rhs);
            }
            break;
        default:
            paw_assert(op == OP_EQ);
            if (!pawV_is_bigint(lhs)) {
                swap(Value, lhs, rhs);
            }
            return bi_eq(pawV_get_bigint(lhs), rhs);
    }
}

void pawB_rel(paw_Env *P, Op op, Value lhs, Value rhs)
{
    paw_assert(pawV_is_bigint(lhs) || pawV_is_bigint(rhs));
    paw_assert(!pawV_is_float(lhs) && !pawV_is_float(rhs));
    paw_assert(!pawV_is_bool(lhs) && !pawV_is_bool(rhs));
    StackPtr sp = pawC_stkinc(P, 1);
    pawV_set_bool(sp, pawB_cmp(op, lhs, rhs));
}

// Evaluate the statement 'bi = bi * factor + offset'
static void mul_add_digit(BigInt *bi, BiDigit factor, BiDigit offset)
{
    BiDigit *out = bi->buf;
    int carry = offset;
    int nz = bi->size;
    BiDigit *z = out;

    for (; nz > 0; --nz, ++z) {
        carry += *z * factor;
        *z = carry & BI_MASK;
        carry >>= BI_BITS;
    }

    if (carry) {
        *z++ = carry;
    }
    bi->size = z - out;
}

void pawB_parse(paw_Env *P, const char *s, int base)
{
    StackPtr sp = pawC_stkinc(P, 1);
    BigInt *bi = pawB_new(P);
    pawV_set_bigint(sp, bi);

    while (ISHEX(*s)) {
        const int offset = HEXVAL(*s++);
        if (offset >= base) {
            pawE_error(P, PAW_ESYNTAX, "invalid integer");
        }
        // bi = bi * base + offset
        bi_ensure(P, bi, bi->size + 1);
        mul_add_digit(bi, base, offset);
    }
    bi_trim(bi);
}
