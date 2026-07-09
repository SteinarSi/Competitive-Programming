#include <iostream>
#include <iomanip>
#include <cstdint>

using namespace std;
using u64 = uint64_t;

const u64 LIMIT = 2000000;
u64 SMALLEST[LIMIT+1];

void preprocess() {
    for (u64 i {0}; i <= LIMIT; SMALLEST[i++] = i);
    for (u64 k {2}; k <= LIMIT; k++) {
        if (SMALLEST[k] == k) {
            for (u64 i {2*k}; i <= LIMIT; i += k) {
                SMALLEST[i] = min(SMALLEST[i], k);
            }
        }
    }
}

u64 solve(u64 x) {
    u64 ret = 1;
    u64 fs = 0;
    while (x >= 2) {
        u64 r = 1;
        fs += 1;
        u64 p = SMALLEST[x];
        do {
            r += 1;
            x /= p;
        }
        while (p == SMALLEST[x]);
        ret *= r;
    }
    return ret - fs;
}

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);

    preprocess();

    u64 q, x;
    cin >> q;
    for (; q --> 0 ;) {
        cin >> x;
        cout << solve(x) << '\n';
    }
}
