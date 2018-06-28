#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>

static int64_t is_prime(int64_t n) {
  if (n < 2) {
    return 0;
  }
  if (n == 2) {
    return 1;
  }
  int64_t i = 2;
  while (i < n) {
    if (n % i == 0) {
      return 0;
    }
    i = i + 1;
  }
  return 1;
}

int main(void) {
  printf("%" PRId64 "\n", is_prime(5000000029));
  return 0;
}
