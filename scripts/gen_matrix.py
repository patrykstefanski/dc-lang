import random
import sys

if len(sys.argv) < 2:
    print('Usage: {} <N> [SEED]'.format(sys.argv[0]))
    sys.exit(1)
n = int(sys.argv[1])
if len(sys.argv) >= 3:
    random.seed(sys.argv[2])
print(n)
for i in range(2 * n * n):
    print(random.randint(-1000, 1000))
