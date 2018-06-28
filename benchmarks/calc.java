import java.util.Scanner;

public class calc {
  private static long[] init(int n, long op) {
    long[] A = new long[n];
    int i = 0;
    while (i < n) {
      A[i] = op;
      i = i + 1;
    }
    return A;
  }

  private static long doCalc(long[] A, int n) {
    long result = 1;
    int i = 0;
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

  public static void main(String[] args) {
    Scanner in = new Scanner(System.in);
    int n = in.nextInt();
    long op = in.nextLong();
    long[] A = init(n, op);
    long result = doCalc(A, n);
    System.out.println(result);
    in.close();
  }
}
