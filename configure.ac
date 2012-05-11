#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
AC_PREREQ([2.65])
AC_INIT([somatic], [0.0.20110909], [ntd@gatech.edu])
AM_INIT_AUTOMAKE

AC_CONFIG_SRCDIR([include/somatic.h])
AC_CONFIG_HEADERS([config.h])


# Checks for programs.
AC_USE_SYSTEM_EXTENSIONS
AC_PROG_CXX
AC_PROG_CC
AC_PROG_LIBTOOL
AC_PROG_CC_C99
AC_CHECK_PROGS([MAXIMA], [maxima])

# Checks for libraries.

# Checks for header files.
AC_CHECK_HEADERS([inttypes.h stdint.h stdlib.h string.h unistd.h math.h threads.h])
AC_CHECK_HEADERS([cblas.h], [true], [exit 1])

# Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_STDBOOL
AC_C_INLINE
AC_TYPE_INT16_T
AC_TYPE_INT32_T
AC_TYPE_INT64_T
AC_C_RESTRICT
AC_TYPE_SIZE_T
AC_TYPE_SSIZE_T
AC_TYPE_UINT16_T
AC_TYPE_UINT32_T
AC_TYPE_UINT64_T
AC_TYPE_UINT8_T
AC_CHECK_TYPES([ptrdiff_t])

# Checks for library functions.
AC_FUNC_ALLOCA
AC_FUNC_MALLOC
AC_FUNC_REALLOC

AC_CHECK_LIB([m],[sqrt])
AC_SEARCH_LIBS([cblas_dgemm],[blas], [true], [echo "Failed: need BLAS library" && exit 1])
AC_SEARCH_LIBS([dgesvd_],[lapack], [true], [echo "Failed: need LAPACK library" && exit 1])
AC_SEARCH_LIBS([clock_gettime],[rt])
AC_SEARCH_LIBS([protobuf_c_message_pack],[protobuf-c])

# Golems libs
AC_SEARCH_LIBS([aa_hard_assert],[amino])
AC_SEARCH_LIBS([ach_open],[ach])

AC_CHECK_FUNCS([clock_gettime memset pow sqrt sincos])

# Doxygen
DX_HTML_FEATURE(ON)
DX_PDF_FEATURE(OFF)
DX_PS_FEATURE(OFF)
DX_INIT_DOXYGEN(amino)

# Enable maximum warnings
AX_CFLAGS_WARN_ALL

dnl APPEND_FLAG( [FLAG] )
dnl Check if FLAG is supported, and if so, add it
AC_DEFUN([APPEND_CFLAG],
         [AX_CHECK_COMPILER_FLAGS([$1], [CFLAGS="$1 $CFLAGS"])])

AC_DEFUN([APPEND_FCFLAG],
         [AX_CHECK_COMPILER_FLAGS([$1], [FCFLAGS="$1 $FCFLAGS"])])

AC_LANG([C])
APPEND_CFLAG([-Wextra])
APPEND_CFLAG([-Wconversion])
APPEND_CFLAG([-Wpointer-arith])
APPEND_CFLAG([-Wfloat-equal])
APPEND_CFLAG([-Wshadow])
APPEND_CFLAG([-Wwrite-strings])
APPEND_CFLAG([-Wc++-compat])

AC_ARG_ENABLE([debug],
        AS_HELP_STRING([--enable-debug], [Build for debugging]))

AS_IF([test "x$enable_debug" = "xyes"],[
    AC_LANG([C])
    APPEND_CFLAG([-fstack-check])
])

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
