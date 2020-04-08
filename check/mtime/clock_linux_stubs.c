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
    caml_invalid_argument("bechamel.clock: unsupported clock");

  return copy_int64(ts.tv_sec * 1000000000LL + ts.tv_nsec);
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
