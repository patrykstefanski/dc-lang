#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

static int64_t* read(int64_t n) {
  int64_t k = n * n;
  int64_t* A = malloc(k * sizeof(*A));
  int64_t i = 0;
  while (i < k) {
    int64_t t = 0;
    scanf("%" PRId64, &t);
    A[i] = t;
    i = i + 1;
  }
  return A;
}

static void write(int64_t* M, int64_t n) {
  int64_t k = n * n;
  int64_t i = 0;
  while (i < k) {
    printf("%" PRId64 "\n", M[i]);
    i = i + 1;
  }
}

static int64_t* mul(int64_t* A, int64_t* B, int64_t n) {
  int64_t size = n * n;
  int64_t* C = malloc(size * sizeof(*A));
  int64_t i = 0;
  while (i < n) {
    int64_t j = 0;
    while (j < n) {
      int64_t sum = 0;
      int64_t k = 0;
      while (k < n) {
        sum = sum + A[i * n + k] * B[k * n + j];
        k = k + 1;
      }
      C[i * n + j] = sum;
      j = j + 1;
    }
    i = i + 1;
  }
  return C;
}

int main() {
  int64_t n = 0;
  scanf("%" PRId64, &n);
  int64_t* A = read(n);
  int64_t* B = read(n);
  int64_t* C = mul(A, B, n);
  write(C, n);
}
