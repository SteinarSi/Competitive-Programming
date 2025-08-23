#include <iostream>
#include <iomanip>
#include <set>

using namespace std;
using i64 = int64_t;

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);
    cout << setprecision(10) << fixed;

    i64 n, q, d, a, i;
    cin >> n >> q;
    multiset<i64> xs;

    for (i = 0; i < n; i++) {
        cin >> d;
        xs.insert(d);
    }
    for (i = 0; i < q; i++) {
        cin >> a >> d;
        if (xs.empty()) {
            cout << -1 << '\n';
        }
        else if (a == 1) {
            auto upper = xs.upper_bound(d);
            if (upper != xs.end()) {
                cout << *upper << '\n';
                xs.erase(upper);
            }
            else {
                cout << -1 << '\n';
            }
        }
        else {
            auto lowereq = xs.find(d);
            if (lowereq != xs.end()) {
                cout << d << '\n';
                xs.erase(lowereq);
            }
            else {
                lowereq = xs.lower_bound(d);
                if (lowereq != xs.begin()) {
                    lowereq--;
                    cout << *lowereq << '\n';
                    xs.erase(lowereq);
                }
                else {
                    cout << -1 << '\n';
                }
            }
        }
    }
}