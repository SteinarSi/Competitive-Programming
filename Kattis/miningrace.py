from math import lgamma, exp


def log_choose(n, k):
    return lgamma(n + 1) - lgamma(k + 1) - lgamma(n - k + 1)


n, b = (int(i) for i in input().split())
print(f"{exp(2 * (log_choose(2 * n, n - b) - log_choose(2 * n, n))):.10f}")
