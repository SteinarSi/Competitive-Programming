#include <iostream>
#include <iomanip>
#include <vector>
#include <algorithm>

using namespace std;

int manhattan(pair<int,int> &a, pair<int,int> &b) {
    return abs(a.first - b.first) + abs(a.second - b.second);
}

int dp(vector<vector<pair<int,int>>> &positions, vector<vector<int>> &opt, int k, int curr) {
    if (curr == k) {
        opt[k] = vector<int>(positions[k].size(), 0);
        return 0;
    }
    dp(positions, opt, k, curr+1);
    int layer_best = 999999999;
    for (pair<int,int> &from : positions[curr]) {
        int best = 99999999;
        for (int j {0}; j < positions[curr+1].size(); j++) {
            best = min(best, manhattan(from, positions[curr+1][j]) + opt[curr+1][j]);
        }
        opt[curr].push_back(best);
        layer_best = min(layer_best, best);
    }
    return layer_best;
}

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);

    int n, k, c;
    cin >> n >> k;
    vector<vector<pair<int,int>>> positions(k+1);
    vector<vector<int>> opt(k+1);

    for (int y {0}; y < n; y++) {
        for (int x {0}; x < n; x++) {
            cin >> c;
            positions[c].push_back({x, y});
        }
    }

    for (int i {1}; i <= k; i++) {
        if (positions[i].size() == 0) {
            cout << -1 << '\n';
            return 0;
        }
    }

    cout << dp(positions, opt, k, 1) << '\n';
}
