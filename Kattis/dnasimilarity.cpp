#include <vector>
#include <iostream>
#include <iomanip>
#include <string>
#include <algorithm>

using namespace std;

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);

    int t, n, m;
    string xs, ys;
    vector<vector<int>> dp(501, vector<int>(501, 0));

    cin >> t;
    for (;t --> 0;) {
        cin >> xs >> ys;
        n = xs.size();
        m = ys.size();
        
        for (int i {1}; i <= n; i++) {
            for (int j {1}; j <= m; j++) {
                if (xs[i-1] == ys[j-1]) dp[i][j] = 1 + dp[i-1][j-1];
                else dp[i][j] = max(dp[i-1][j], dp[i][j-1]);
            }
        }
        cout << dp[n][m] << '\n';
    }
}
