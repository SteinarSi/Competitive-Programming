#include <iostream>
#include <iomanip>
#include <vector>
#include <algorithm>

using namespace std;

void solve() {
    int n, t, m, h, p;
    cin >> n >> t >> m;
    t--;
    vector<vector<int>> cars(n);
    vector<int> employees(n, 0);

    for (int j {0}; j < m; j++) {
        cin >> h >> p;
        h--;
        employees[h]++;
        if (p > 0) cars[h].push_back(p);
    }
    vector<int> ret(n, 0);
    for (int j {0}; j < n; j++) {
        if (j == t) continue;
        sort(cars[j].begin(), cars[j].end());
        for (int k {cars[j].size()-1}; k >= 0 && employees[j] > 0; k--) {
            ret[j] += 1;
            employees[j] -= cars[j][k];
        }
        if (employees[j] > 0) {
            cout << "IMPOSSIBLE\n";
            return;
        }
    }
    for (int r : ret) {
        cout << r << ' ';
    }
    cout << '\n';
}

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);
    cout << setprecision(10) << fixed;

    int c;
    cin >> c;
    for (int i {1}; i <= c; i++) {
        cout << "Case #" << i << ": ";
        solve();
    }
}