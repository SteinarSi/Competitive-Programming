#include <iostream>
#include <iomanip>
#include <vector>
#include <algorithm>

using namespace std;

void branch(int n, int curr, int diff, int mask, int count, vector<int> &xs, int &best, vector<bool> &seen) {
    if (diff < -1000 || diff > 1000) return;
    if (curr >= best) return;
    if (count >= n) {
        if (curr < best) best = curr;
        return;
    }

    int hash = mask * 1001 + diff;
    if (seen[hash]) return;
    seen[hash] = true;

    for (int i {0}; i < n; i++) {
        if (mask & (1 << i)) continue;

        int x = xs[i];
        branch(n, max(curr,abs(diff + x)), abs(diff + x), mask | (1 << i), count+1, xs, best, seen);
        branch(n, max(curr,abs(diff - x)), abs(diff - x), mask | (1 << i), count+1, xs, best, seen);
    }
}

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);

    int n;
    cin >> n;
    vector<int> xs(n);

    for (int i {0}; i < n; i++) {
        cin >> xs[i];
    }

    int best = 0;
    sort(xs.begin(), xs.end(), greater<>());
    int diff = 0;
    for (int i {n-1}; i >= 0; i--) {
        if (diff < 0) diff += xs[i];
        else diff -= xs[i];
        best = max(best, abs(diff));
    }
    vector<bool> seen((1 << 16) * 1001);

    branch(n, 0, 0, 0, 0, xs, best, seen);

    cout << best << '\n';
}
