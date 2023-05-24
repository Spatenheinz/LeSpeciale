#include "syscall.h"

#include <unistd.h>
#include <sys/syscall.h>

int main(void) {
  syscall(451);
  return 0;
}
