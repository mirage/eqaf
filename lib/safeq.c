#include "caml/mlvalues.h"

CAMLprim value
eqaf(value s1, value s2)
{
  mlsize_t ln = Wosize_val(s1); /* assert (Wosize_val(s1) == Wosize_val(s2)); */
  value * p1, * p2;
  int rt = 0;

  for (p1 = Op_val(s1), p2 = Op_val(s2); ln > 0; ln--, p1++, p2++)
    rt |= *p1 ^ *p2;

  return Val_bool(rt == 0);
}
