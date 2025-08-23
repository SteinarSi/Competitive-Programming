#include <iostream>
#include <vector>
#include <iomanip>
#include <cstdint>
#include <algorithm>

using namespace std;
using i64 = int64_t;

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);
    cout << setprecision(10) << fixed;

    int n, m;
    cin >> n >> m;
    vector<int> knowns(n);
    for (int i {0}; i < n; cin >> knowns[i++]);
    vector<vector<i64>> dp(n+1, vector<i64>(m+1));

    for (int p {1}; p <= m; p++) {
        dp[n][p] = 1;
    }
    for (int i {n-1}; i >= 0; i--) {
        for (int p {1}; p <= m; p++) {
            i64 known = knowns[i];
            i64 ret {0};
            if (known != 0) {
                if (abs(p-known) > 1) ret = 0;
                else if (i == n-1) ret = 1;
                else ret = dp[i+1][known];
            }
            else {
                int s, t;
                if (i == 0) {
                    s = 1;
                    t = m;
                }
                else {
                    s = max(1, p-1);
                    t = min(m, p+1);
                }
                for (int q {s}; q <= t; q++) {
                    ret += dp[i+1][q];
                }
                ret %= 1000000007;
            }
            dp[i][p] = ret;
        }
    }
    if (knowns[0]) cout << dp[0][knowns[0]] << '\n';
    else cout << dp[0][1] << '\n';
}
