#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <errno.h>
#include <stdint.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>

#ifndef __unused
#define __unused(x) x __attribute((unused))
#endif
#define __unit() value __unused(unit)

CAMLprim value
clock_linux_get_time_byte(__unit ())
{
  struct timespec ts;

  if (clock_gettime(CLOCK_MONOTONIC, &ts))
    caml_invalid_argument("clock: unsupported clock");

  return copy_int64(ts.tv_sec * 1000000000LL + ts.tv_nsec);
}

uint64_t
clock_linux_get_tick(__unit ())
{
#if defined(___i386__)
  int64_t ret;
  __asm__ __volatile__ ("rdtsc" : "=A"(ret));
  return ret;
#elif defined(__x86_64__) || defined(__amd64__)
  uint64_t hi, lo;
  __asm__ __volatile__ ("rdtsc" : "=a"(lo), "=d"(hi));
  return (lo | (hi << 32));
#elif defined(__powerpc__) || defined(__ppc__)

#if defined(__powerpc64__) || defined(__ppc64__)
  int64_t tb;
  __asm__ __volatile__("mfspr %0, 268" : "=r" (tb));
  return tb;
#else
  uint32_t tbl, tbu0, tbu1;
  __asm__ __volatile__(
    "mftbu %0\n"
    "mftbl %1\n"
    "mftbu %2"
    : "=r"(tbu0), "=r"(tbl), "=r"(tbu1));
  tbl &= (int32_t)(tbu0 == tbu1);
  return (((uint64_t) tbu1 << 32) | ((uint64_t) tbl));
#endif

#elif defined(__ia64__)
  int64_t itc;
  __asm__("mov %0 = ar.itc" : "=r"(itc));
  return itc;
#elif defined(__aarch64__)
  int64_t virtual_timer_value;
  __asm__ __volatile__("mrs %0, cntvct_el0" : "=r"(virtual_timer_value));
  return virtual_timer_value;
#else
#error A Cycle Timer is not available for your OS and CPU
#endif
  // XXX(dinosaure): [clock_gettime] costs a lot and are
  // not really precise. [rdtsc] (Read Time Stamp Counter)
  // is more reliable.
}

uint64_t
clock_linux_get_time_native(__unit ())
{
  struct timespec ts;

  (void) clock_gettime(CLOCK_MONOTONIC, &ts);
  // XXX(dinosaure): assume that it will never fail.
  // [caml_invalid_argument] allocs.

  return (ts.tv_sec * 1000000000LL + ts.tv_nsec);
}
