#include <wtypes.h>
#include <winbase.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <io.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <ctype.h>
#include <errno.h>
#include <string.h>
#include <signal.h>

#include "caml/alloc.h"
#include "caml/address_class.h"
#include "caml/fail.h"
#include "caml/io.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/osdeps.h"
#include "caml/signals.h"
#include "caml/sys.h"
#include "caml/startup_aux.h"

static LARGE_INTEGER frequency;

CAMLprim value
clock_windows_init(value __unused(unit))
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
