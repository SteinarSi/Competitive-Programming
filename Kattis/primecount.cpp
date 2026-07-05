#include <iostream>
#include <cstdint>
#include <vector>
#include <cmath>

using namespace std;
using u64 = uint64_t;

const u64 MAX_S {100};
const u64 MAX_X {100010};
const u64 MAX_P {10000010};

vector<u64> primes {2};
u64 phi_cache[MAX_S][MAX_X];
u64 pi_cache[MAX_P+1];

void fill_caches() {
    vector<char> prime((MAX_P+1)/2,true);
    pi_cache[0] = 0;
    pi_cache[1] = 0;
    pi_cache[2] = 1;
    for (u64 p {3}; p < MAX_P; p += 2) {
        if (prime[p >> 1]) {
            primes.push_back(p);
            for (u64 q {p*p}; q < MAX_P; q += p+p) {
                prime[q >> 1] = false;
            }
            pi_cache[p] = pi_cache[p-1] + 1; 
        }
        else pi_cache[p] = pi_cache[p-1];
        pi_cache[p+1] = pi_cache[p];
    }

    for (u64 x {0}; x < MAX_X; x++) phi_cache[0][x] = x;
    for (u64 s {1}; s < MAX_S; s++) {
        for (u64 x {0}; x < MAX_X; x++) {
            phi_cache[s][x] = phi_cache[s-1][x] - phi_cache[s-1][x / primes[s-1]];
        }
    }
}

u64 phi(u64 x, u64 s) {
    if (s == 0) return x;
    if (x < MAX_X && s < MAX_S) return phi_cache[s][x];
    if (x < MAX_P && primes[s-1]*primes[s-1] >= x) return pi_cache[x] - s + 1;

    return phi(x,s-1) - phi(x / primes[s-1], s-1);
}

u64 pi(u64 x) {
    if (x < MAX_P) return pi_cache[x];

    u64 s = sqrt(0.5 + x);
    u64 a = pi_cache[(u64) cbrt(0.5 + x)];

    u64 ret {phi(x, a) + a - 1};
    for (u64 i {a}; primes[i] <= s; i++) {
        ret = ret - pi(x / primes[i]) + pi(primes[i]) - 1;
    }
    return ret;
}

int main() {
    u64 n;
    cin >> n;

    fill_caches();

    cout << pi(n) << '\n';
}
