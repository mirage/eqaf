#ifdef __MACH__
#include <mach/mach.h>
#include <mach/mach_time.h>
#include <unistd.h>
#endif

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>

CAMLprim value
clock_mach_get_time(value unit)
{
  CAMLparam0();

#ifdef __MACH__
  static mach_timebase_info_data_t s;
  uint64_t now;

  now = mach_absolute_time();

  if (s.denom == 0)
    (void) mach_timebase_info(&s);

  CAMLreturn(copy_int64(now * s.numer / s.denom));
#else
  /* XXX(dinosaure): should not appear. */
  CAMLreturn(copy_int64(0L));
#endif
}
