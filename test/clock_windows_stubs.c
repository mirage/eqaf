#include <winbase.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>

static LARGE_INTEGER frequency;

CAMLprim value
clock_windows_init(value _unit)
{
  QueryPerformanceFrequency(&frequency);
  frequency.QuadPart = 1000000000L / frequency.QuadPart;

  CAMLreturn(Val_unit);
}

CAMLprim value
clock_windows_get_time(value __unused(unit))
{
  CAMLlocal1(res);
  LARGE_INTEGER now;

  QueryPerformanceCounter(&now);

  res = copy_int64(now.QuadPart * frequency.QuadPart);

  CAMLreturn(res);
}
