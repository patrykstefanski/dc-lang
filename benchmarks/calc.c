#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

static int64_t* init(int64_t n, int64_t op) {
  int64_t* A = malloc(n * sizeof(*A));
  int64_t i = 0;
  while (i < n) {
    A[i] = op;
    i = i + 1;
  }
  return A;
}

static int64_t calc(int64_t* A, int64_t n) {
  int64_t result = 1;
  int64_t i = 0;
  while (i < n) {
    if (A[i] == 0) {
      result = result + 1;
    } else if (A[i] == 1) {
      result = result - 1;
    } else if (A[i] == 2) {
      result = result * i;
    } else if (A[i] == 3) {
      result = result / i;
    } else if (A[i] == 4) {
      result = result + i;
    } else if (A[i] == 5) {
      result = result - i;
    } else {
      result = 0;
    }
    i = i + 1;
  }
  return result;
}

int main(void) {
  int64_t n = 0;
  scanf("%" PRId64, &n);
  int64_t op = 0;
  scanf("%" PRId64, &op);
  int64_t* A = init(n, op);
  int64_t result = calc(A, n);
  printf("%" PRId64 "\n", result);
}
