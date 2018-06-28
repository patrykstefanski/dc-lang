#ifndef ASSERT_HPP
#define ASSERT_HPP

#include <cstdio>
#include <cstdlib>

#include "cxx_extensions.hpp"

#ifndef NDEBUG

#define ASSERT(expr)                                                          \
  do {                                                                        \
    if (UNLIKELY(!(expr))) {                                                  \
      std::fprintf(stderr, "Asseration \"%s\" failed in %s (%s:%u)\n", #expr, \
                   __func__, __FILE__, __LINE__);                             \
      std::abort();                                                           \
    }                                                                         \
  } while (0)

#define _ASSERT_OP(op, a, b)                                           \
  do {                                                                 \
    if (UNLIKELY(!((a)op(b)))) {                                       \
      std::fprintf(stderr, "Asseration \"%s\" failed in %s (%s:%u)\n", \
                   #a " " #op " " #b, __func__, __FILE__, __LINE__);   \
      std::abort();                                                    \
    }                                                                  \
  } while (0)

#define ASSERT_EQ(a, b) _ASSERT_OP(==, a, b)
#define ASSERT_NE(a, b) _ASSERT_OP(!=, a, b)
#define ASSERT_LT(a, b) _ASSERT_OP(<, a, b)
#define ASSERT_LE(a, b) _ASSERT_OP(<=, a, b)
#define ASSERT_GT(a, b) _ASSERT_OP(>, a, b)
#define ASSERT_GE(a, b) _ASSERT_OP(>=, a, b)

#else  // NDEBUG

#define ASSERT(expr) ((void)0)
#define ASSERT_EQ(a, b) ((void)0)
#define ASSERT_NE(a, b) ((void)0)
#define ASSERT_LT(a, b) ((void)0)
#define ASSERT_LE(a, b) ((void)0)
#define ASSERT_GT(a, b) ((void)0)
#define ASSERT_GE(a, b) ((void)0)

#endif  // !NDEBUG

#endif  // !ASSERT_HPP
