#include <iostream>
#include <iomanip>
#include <cstdint>
#include <vector>

using namespace std;
using u64 = uint64_t;
using u128 = __uint128_t;

vector<u64> BASES = {2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37};

u64 powmod(u64 base, u64 e, u64 mod) {
    u64 result = 1;
    base %= mod;
    while (e) {
        if (e & 1) result = ((u128)result * base) % mod;
        base = ((u128)base * base) % mod;
        e >>= 1;
    }
    return result;
}

bool check_composite(u64 n, u64 a, u64 d, int s) {
    u64 x = powmod(a, d, n);
    if (x == 1 || x == n - 1) return false;
    for (;s --> 1;) {
        x = ((u128)x * x) % n;
        if (x == n - 1) return false;
    }
    return true;
};

bool is_prime(u64 n) {
    if (n < 2) return false;
    for (u64 a : BASES) {
        if (a == n) return true;
        if (n % a == 0) return false;
    }

    int r = 0;
    u64 d = n - 1;
    while ((d & 1) == 0) {
        d >>= 1;
        r++;
    }

    for (u64 a : BASES) {
        if (n == a) return true;
        if (check_composite(n, a, d, r)) return false;
    }
    return true;
}

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);

    u64 n;
    cin >> n;

    if (is_prime(n)) cout << "YES\n";
    else cout << "NO\n";
}
