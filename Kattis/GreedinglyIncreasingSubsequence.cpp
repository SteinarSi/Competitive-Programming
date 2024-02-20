#include <iostream>
#include <vector>
#include <iomanip>
#include <cstdint>

using namespace std;
using u64 = uint64_t;

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);

    u64 n, g;
    cin >> n;
    u64 m = 0;
    u64 c = 0;
    vector<u64> inc(1000001);
    for (u64 i {0}; i < n; i++) {
        cin >> g;
        if (g > m) {
            inc[c++] = g;
            m = g;
        }
    }
    cout << c << '\n';
    for (u64 i {0}; i < c; cout << inc[i++] << ' ');
    cout << endl;
}
