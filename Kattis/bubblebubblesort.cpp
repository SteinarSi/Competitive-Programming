#include <iostream>
#include <vector>
#include <iomanip>
#include <algorithm>

using namespace std;

bool is_sorted(vector<int> &xs) {
    for (int i {0}; i < xs.size()-1; i++) {
        if (xs[i] > xs[i+1]) return false;
    }
    return true;
}

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);

    int n, k;
    cin >> n >> k;

    vector<int> xs(n);
    for (int i {0}; i < n; cin >> xs[i++]);

    int ret {0};
    while ( ! is_sorted(xs)) {
        for (int i {0}; i <= n-k; i++) {
            sort(xs.begin() + i, xs.begin() + i + k);
        }
        ret++;
    }

    cout << ret << '\n';
}
