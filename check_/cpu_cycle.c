#include <caml/mlvalues.h>

#ifndef __unused
#define __unused(x) x __attribute((unused))
#endif
#define __unit() value __unused(unit)

uint64_t
caml_cpu_cycles(__unit ())
{
  unsigned int hi, lo;
  __asm__ volatile ("rdtsc\n\t" : "=a"(lo), "=d"(hi));
  return ((uint64_t) lo) | (((uint64_t) hi) << 32);
}
