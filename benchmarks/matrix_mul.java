import java.util.Scanner;

public class matrix_mul {
  private static long[] read(Scanner in, int n) {
    int k = n * n;
    long[] A = new long[k];
    int i = 0;
    while (i < k) {
      long t = in.nextLong();
      A[i] = t;
      i = i + 1;
    }
    return A;
  }

  private static void write(long[] M, int n) {
    int k = n * n;
    int i = 0;
    while (i < k) {
      System.out.println(M[i]);
      i = i + 1;
    }
  }

  private static long[] mul(long[] A, long[] B, int n) {
    int size = n * n;
    long[] C = new long[size];
    int i = 0;
    while (i < n) {
      int j = 0;
      while (j < n) {
        long sum = 0;
        int k = 0;
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

  public static void main(String[] args) {
    Scanner in = new Scanner(System.in);
    int n = in.nextInt();
    long[] A = read(in, n);
    long[] B = read(in, n);
    long[] C = mul(A, B, n);
    write(C, n);
    in.close();
  }
}
