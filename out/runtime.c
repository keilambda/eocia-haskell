#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <inttypes.h>

int read_int() {
  int64_t n;
  if (!scanf("%" SCNd64, &n)) {
    fprintf(stderr, "read_int: invalid input\n");
    exit(1);
  }
  return n;
}

void print_int(int64_t x) {
  printf("%" PRId64 "\n", x);
}

__attribute__((noreturn))
void prelude();

int main() {
  prelude();
}
