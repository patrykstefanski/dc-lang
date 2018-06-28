public class prime {
  private static long isPrime(long n) {
    if (n < 2) {
      return 0;
    }
    if (n == 2) {
      return 1;
    }
    long i = 2;
    while (i < n) {
      if (n % i == 0) {
        return 0;
      }
      i = i + 1;
    }
    return 1;
  }

  public static void main(String[] args) {
    System.out.println(isPrime(5000000029L));
  }
}
