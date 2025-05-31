#include <iostream>
#include <iomanip>
#include <vector>
#include <cstdint>

using namespace std;
using i64 = int64_t;

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);
    cout << setprecision(10) << fixed;

    i64 n;
    cin >> n;

    if (n <= 2) {
        cout << 0 << '\n';
        return 0;
    }

    i64 ret = 2;
    vector<bool> sieve(n,true);
    sieve[0] = false;
    sieve[1] = false;
    for (int i {4}; i < n; i += 2) {
        sieve[i] = false;
    }
    for (int i {3}; i < n; i += 2) {
        if (sieve[i]) {
            ret += i;
            for (int j {i+i}; j < n; j += i) {
                sieve[j] = false;
            }
        }
    }

    cout << ret << '\n';
}
