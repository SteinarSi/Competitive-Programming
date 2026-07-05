#include <iostream>
#include <iomanip>
#include <cstdint>
#include <vector>
#include <cmath>

using namespace std;
using u64 = uint64_t;
using u128 = __uint128_t;

vector<u64> BASES = {2, 3, 5, 7};
static const u64 wheel_residues[] = {1, 7, 11, 13, 17, 19, 23, 29};
static const u64 wheel[] = {6, 4, 2, 4, 2, 4, 6, 2};
const u64 S {100000};

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

bool composite_check(u64 n, u64 a, u64 d, int s) {
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
        if (composite_check(n, a, d, r)) return false;
    }
    return true;
}

int pi(u64 n) {
    u64 root = sqrt(n);
    u64 ret = (n >= 2) + (n >= 3) + (n >= 5);

    vector<u64> primes {3,5};

    vector<char> prime((root+2)/2,true);
    for (u64 p = 7, wi = 1; p <= root; p += wheel[wi], wi = (wi+1)&7) {
        if ( ! prime[p >> 1]) continue;
        primes.push_back(p);
        for (u64 q {p*p}; q <= root; q += p+p) {
            if (q & 1) prime[q >> 1] = false;
        }
    }

    vector<char> block(S/2);
    for (u64 k {0}; k*S <= n; k++) {
        fill(block.begin(),block.end(),true);
        u64 start = k * S;
        for (u64 p : primes) {
            u64 idx = max((start + p - 1) / p, p) * p - start;
            if ((idx & 1) == 0) idx += p;
            for (u64 j {idx}; j < S; j += p+p) {
                block[j >> 1] = false;
            }
        }

        u64 r = start % 30;
        u64 wi = 0;
        while (wi < 8 && wheel_residues[wi] < r) wi++;

        for (u64 i {wheel_residues[wi] - r}; i < S && start + i <= n; i += wheel[wi], wi = (wi+1)&7) {
            if (start + i < 2) continue;
            ret += block[i >> 1];
        }
    }

    return ret;
}

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);

    u64 n, q, x;
    cin >> n >> q;

    cout << pi(n) << '\n';
    for (; q --> 0 ;) {
        cin >> x;
        cout << is_prime(x) << '\n';
    }
}
