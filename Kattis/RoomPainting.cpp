#include <iostream>
#include <vector>
#include <iomanip>
#include <set>

using namespace std;
using i64 = int64_t;

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);

    i64 n, m, x, y;
    cin >> n >> m;
    set<i64> cans;

    for (i64 i {n}; i --> 0;) {
        cin >> x;
        cans.insert(x);
    }

    i64 ret {0};
    for (i64 i {m}; i --> 0;) {
        cin >> y;
        ret += *cans.lower_bound(y) - y;
    }
    cout << ret << '\n';
}
