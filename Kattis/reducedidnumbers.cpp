#include <iostream>
#include <iomanip>
#include <set>
#include <vector>

using namespace std;
using i64 = int64_t;

bool valid(vector<int> &xs, int m) {
    set<int> seen;
    for (int x : xs) {
        int y = x % m;
        if (seen.count(y)) return false;
        seen.insert(y);
    }
    return true;
}

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);
    cout << setprecision(10) << fixed;

    int n;
    cin >> n;
    vector<int> xs(n);
    for (int i {0}; i < n; cin >> xs[i++]);

    int m = n;
    while ( ! valid(xs, m)) m++;

    cout << m << '\n';
}
