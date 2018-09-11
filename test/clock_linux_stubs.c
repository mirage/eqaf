#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <errno.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>

#define option_none Val_int(0)

CAMLprim value
option_some(value v)
{
  CAMLparam1(v);
  CAMLlocal1(res);

  res = caml_alloc(1, 0);
  Store_field(res, 0, v);

  CAMLreturn(res);
}

CAMLprim value
clock_linux_get_clock_id(value unit)
{
  CAMLparam1(unit);
  CAMLlocal1(res);

  res = caml_alloc_tuple(8);
  Store_field(res, 0, Val_int(CLOCK_REALTIME));
  Store_field(res, 1, Val_int(
#ifdef CLOCK_REALTIME_COARSE
                              CLOCK_REALTIME_COARSE
#elif defined CLOCK_REALTIME_FAST
                              CLOCK_REALTIME_FAST
#else
                              CLOCK_REALTIME
#endif
                              ));
  Store_field(res, 2, Val_int(CLOCK_MONOTONIC));
  Store_field(res, 3, Val_int(
#ifdef CLOCK_MONOTONIC_COARSE
                              CLOCK_MONOTONIC_COARSE
#elif defined CLOCK_MONOTONIC_FAST
                              CLOCK_MONOTONIC_FAST
#else
                              CLOCK_MONOTONIC
#endif
                              ));
  Store_field(res, 4, Val_int(
#ifdef CLOCK_MONOTONIC_RAW
                              CLOCK_MONOTONIC_RAW
#else
                              CLOCK_MONOTONIC
#endif
                              ));
  Store_field(res, 5, Val_int(
#ifdef CLOCK_BOOTTIME
                              CLOCK_BOOTTIME
#else
                              CLOCK_MONOTONIC
#endif
                              ));
  Store_field(res, 6,
#ifdef CLOCK_PROCESS_CPUTIME_ID
              option_some(Val_int(CLOCK_PROCESS_CPUTIME_ID))
#else
              option_none
#endif
              );
  Store_field(res, 7,
#ifdef CLOCK_THREAD_CPUTIME_ID
              option_some(Val_int(CLOCK_THREAD_CPUTIME_ID))
#else
              option_none
#endif
              );

  CAMLreturn(res);
}

CAMLprim value
clock_linux_get_time(value v_clock_id)
{
  CAMLparam1(v_clock_id);
  CAMLlocal1(res);

  clockid_t clock_id = Int_val(v_clock_id);
  struct timespec ts;

  if (clock_gettime(clock_id, &ts) != 0) {
    switch (errno) {
    case EINVAL:
      /* not supported */
      caml_invalid_argument ("Unsupported clock");
    case EFAULT:
      /* invalid ts */
    default:
      caml_failwith("unknown failure");
    }
  }

  res = copy_int64(ts.tv_sec * 1000000000LL + ts.tv_nsec);

  CAMLreturn(res);
}
