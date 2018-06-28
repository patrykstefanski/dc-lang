#ifndef CXX_EXTENSIONS_HPP
#define CXX_EXTENSIONS_HPP

// Feature checking macros.

#ifdef __has_attribute
#define _HAS_ATTRIBUTE(attr) __has_attribute(attr)
#else
#define _HAS_ATTRIBUTE(attr) 0
#endif

#ifdef __has_builtin
#define _HAS_BUILTIN(builtin) __has_builtin(builtin)
#else
#define _HAS_BUILTIN(builtin) 0
#endif

// Builtins.

#if _HAS_BUILTIN(__builtin_expect)
#define LIKELY(exp) __builtin_expect((exp), 1)
#define UNLIKELY(exp) __builtin_expect((exp), 0)
#else
#define LIKELY(exp) (exp)
#define UNLIKELY(exp) (exp)
#endif

#if _HAS_BUILTIN(__builtin_unreachable)
#define UNREACHABLE() __builtin_unreachable()
#else
#define UNREACHABLE() /* __builtin_unreachable() */
#endif

// Function attributes.

#if _HAS_ATTRIBUTE(__cold__)
#define COLD __attribute__((__cold__))
#else
#define COLD /* __attribute__((__cold__)) */
#endif

// Undefine the feature checking macros.

#undef _HAS_ATTRIBUTE
#undef _HAS_BUILTIN

#endif  // !CXX_EXTENSIONS_HPP
