#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <asm/unistd.h>
#include <sys/prctl.h>
#include <sys/ioctl.h>

#include <linux/perf_event.h>
#include <linux/hw_breakpoint.h>

#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <caml/memory.h>

long
perf_event_open(struct perf_event_attr *hw_event, pid_t pid, int cpu, int group_fd, unsigned long flags)
{
  int rt;

  rt = syscall(__NR_perf_event_open, hw_event, pid, cpu, group_fd, flags);

  return rt;
}

CAMLprim value
perf_linux_event_enable(value fd)
{
  CAMLparam1(fd);

  int rt;

  rt = ioctl(Int_val(fd), PERF_EVENT_IOC_ENABLE);

  if (rt == -1)
    caml_failwith(strerror(errno));

  CAMLreturn(Val_unit);
}

CAMLprim value
perf_linux_event_disable(value fd)
{
  CAMLparam1(fd);

  int rt;

  rt = ioctl(Int_val(fd), PERF_EVENT_IOC_DISABLE);

  if (rt == -1)
    caml_failwith(strerror(errno));

  CAMLreturn(Val_unit);
}

CAMLprim value
perf_linux_event_open(value v_kind, value v_attr_flags, value v_pid, value v_cpu, value v_group_fd, value v_flags)
{
  CAMLparam5(v_kind, v_attr_flags, v_pid, v_cpu, v_group_fd);
  CAMLxparam1(v_flags);

  int rt;
  int c_flags = 0;

  struct perf_event_attr attr;
  memset(&attr, 0, sizeof(struct perf_event_attr));
  attr.size = sizeof(struct perf_event_attr);
#ifdef PERF_FLAG_FD_CLOEXEC
  if (Int_val(v_flags) & 1) c_flags += PERF_FLAG_FD_CLOEXEC;
#endif
  if (Int_val(v_flags) & 2) c_flags += PERF_FLAG_FD_NO_GROUP;
  if (Int_val(v_flags) & 4) c_flags += PERF_FLAG_FD_OUTPUT;
  if (Int_val(v_flags) & 8) c_flags += PERF_FLAG_PID_CGROUP;

  if (Int_val(v_kind) < 10) attr.type = PERF_TYPE_HARDWARE;
  else attr.type = PERF_TYPE_SOFTWARE;

  switch (Int_val(v_kind))
    {
    case 0: attr.config = PERF_COUNT_HW_CPU_CYCLES; break;
    case 1: attr.config = PERF_COUNT_HW_INSTRUCTIONS; break;
    case 2: attr.config = PERF_COUNT_HW_CACHE_REFERENCES; break;
    case 3: attr.config = PERF_COUNT_HW_CACHE_MISSES; break;
    case 4: attr.config = PERF_COUNT_HW_BRANCH_INSTRUCTIONS; break;
    case 5: attr.config = PERF_COUNT_HW_BRANCH_MISSES; break;
    case 6: attr.config = PERF_COUNT_HW_BUS_CYCLES; break;
    case 7: attr.config = PERF_COUNT_HW_STALLED_CYCLES_FRONTEND; break;
    case 8: attr.config = PERF_COUNT_HW_STALLED_CYCLES_BACKEND; break;
    case 9: attr.config = PERF_COUNT_HW_REF_CPU_CYCLES; break;
    case 10: attr.config = PERF_COUNT_SW_CPU_CLOCK; break;
    case 11: attr.config = PERF_COUNT_SW_TASK_CLOCK; break;
    case 12: attr.config = PERF_COUNT_SW_PAGE_FAULTS; break;
    case 13: attr.config = PERF_COUNT_SW_CONTEXT_SWITCHES; break;
    case 14: attr.config = PERF_COUNT_SW_CPU_MIGRATIONS; break;
    case 15: attr.config = PERF_COUNT_SW_PAGE_FAULTS_MIN; break;
    case 16: attr.config = PERF_COUNT_SW_PAGE_FAULTS_MAJ; break;
    case 17: attr.config = PERF_COUNT_SW_ALIGNMENT_FAULTS; break;
    case 18: attr.config = PERF_COUNT_SW_EMULATION_FAULTS; break;
    default: attr.config = PERF_COUNT_SW_DUMMY; break;
    }

  if (Int_val(v_attr_flags) & 1) attr.disabled = 1;
  if (Int_val(v_attr_flags) & 2) attr.inherit = 1;
  if (Int_val(v_attr_flags) & 4) attr.exclude_user = 1;
  if (Int_val(v_attr_flags) & 8) attr.exclude_kernel = 1;
  if (Int_val(v_attr_flags) & 16) attr.exclude_hv = 1;
  if (Int_val(v_attr_flags) & 32) attr.exclude_idle = 1;
  if (Int_val(v_attr_flags) & 64) attr.enable_on_exec = 1;

  rt = perf_event_open(&attr, Int_val(v_pid), Int_val(v_cpu), Int_val(v_group_fd), c_flags);

  if (rt == -1)
    caml_failwith(strerror(errno));

  CAMLreturn(Val_int(rt));
}

CAMLprim value
perf_linux_event_open_byte (value *argv, int argn)
{
  return perf_linux_event_open(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}
