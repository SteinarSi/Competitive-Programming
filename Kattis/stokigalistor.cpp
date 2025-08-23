#include <vector>
#include <algorithm>
#include <iomanip>
#include <iostream>

using namespace std;

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);

    int n;
    cin >> n;
    vector<int> xs(n);
    vector<int> ys(n);
    for (int i {0}; i < n; i++) {
        cin >> xs[i];
        ys[i] = xs[i];
    }

    sort(ys.begin(), ys.end());

    int ret = 0;
    for (int i {0}; i < n; i++) {
        ret += xs[i] != ys[i];
    }

    cout << ret << '\n';
}
