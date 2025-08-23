#include <iostream>
#include <iomanip>

using namespace std;
using i64 = int64_t;

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);
    cout << setprecision(10) << fixed;

    i64 s, a, b;
    cin >> s >> a >> b;
    
    for (int i {s / a}; i > 0; i--) {
        if ((s - (a*i)) % b == 0) {
            cout << i << ' ' << (s - (a*i)) / b << '\n';
            return 0;
        }
    }
    if (s % b == 0) {
        cout << 0 << ' ' << s / b << '\n';
    }
    else {
        cout << "Impossible" << '\n';
    }
}
