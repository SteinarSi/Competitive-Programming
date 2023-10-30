#include <algorithm>
#include <cassert>
#include <cstdint>
#include <iostream>
#include <set>
#include <string>
#include <vector>

using namespace std;

#define reps(i, s, n) for (int i = s; i < n; ++i)
#define rep(i, n) reps(i, 0, n)

int main() {
    cin.tie(nullptr)->sync_with_stdio(false);

    int n, m;
    cin >> n >> m;
    auto map = vector<string>(n);
    for (auto &row : map) cin >> row;

    auto _dp = vector<int>(n * m, 0);
    auto dp = [&](int y, int x) -> int & {
        assert(0 <= y && y < n);
        assert(0 <= x && x < m);
        return _dp[y * m + x];
    };

    rep(y, n) {
        if (map[y][0] == '.') dp(y, 0) = 1;
    }
    rep(x, m) {
        if (map[0][x] == '.') dp(0, x) = 1;
    }
    reps(y, 1, n) {
        reps(x, 1, m) {
            if (map[y][x] != '#') dp(y, x) = min({dp(y - 1, x), dp(y, x - 1), dp(y - 1, x - 1)}) + 1;
        }
    }

    auto kill = vector<std::basic_string<int>>(max(n, m));
    auto active = std::multiset<int>();

    rep(y, n) {
        active.clear();
        for (int x = m; x-- > 0;) {
            int l = x - dp(y, x);
            active.insert(dp(y, x));
            if (l >= 0) {
                kill[l].push_back(dp(y, x));
            }

            for (auto v : kill[x]) active.erase(active.find(v));
            kill[x].clear();

            if (!active.empty()) dp(y, x) = *active.rbegin();
        }
    }

    rep(x, m) {
        active.clear();
        for (int y = n; y-- > 0;) {
            int u = y - dp(y, x);
            active.insert(dp(y, x));
            if (u >= 0) {
                kill[u].push_back(dp(y, x));
            }

            for (auto v : kill[y]) active.erase(active.find(v));
            kill[y].clear();

            if (!active.empty()) dp(y, x) = *active.rbegin();
        }
    }

    int q;
    cin >> q;
    for (int i{0}; i < q; i++) {
        int y, x;
        cin >> y >> x;
        --y, --x;
        int s = dp(y, x);
        cout << s * s << '\n';
    }
}
