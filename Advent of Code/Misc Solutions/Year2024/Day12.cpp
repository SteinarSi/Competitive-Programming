// TODO: need to update the parser


#pragma GCC optimize("O3")

#include <iostream>
#include <string>
#include <vector>
#include <iomanip>
#include <map>
#include <queue>
#include <set>
#include <algorithm>
#include <cstdint>
#include <fstream>

using namespace std;
using i64 = int64_t;
using u64 = uint64_t;

i64 solve(string &text, vector<int> &cs, vector<vector<vector<i64>>> &dp, int x, int n, int c) {
    // cout << "Entering " << x << ',' << n << ',' << c << '\n';
    if (dp[x][n][c] != -1) return dp[x][n][c];
    i64 ret {-1};
    char at = text[max(x-1, 0)];
    if (x == 0 && c == 0) {
        if (n == 0) ret = 1;
        else ret = 0;
    }
    else if (x == 0 && c == 1) {
        if (n == cs[c]) ret = 1;
        else ret = 0;
    }
    else if (x == 0 && c >= 2) ret = 0;
    // else if (n > cs[c]) ret = 0;
    else if (x > 0 && c == 0) {
        if (at == '#') ret = 0;
        else ret = solve(text, cs, dp, x-1, 0, 0);
    }
    else if (at == '.') {
        if (n == 0) ret = solve(text, cs, dp, x-1, 0, c);
        else if (n == cs[c]) ret = solve(text, cs, dp, x-1, 0, c-1);
        else ret = 0;
    }
    else if (at == '#') {
        if (n >= cs[c]) ret = 0;
        else ret = solve(text, cs, dp, x-1, n+1, c);
    }
    else if (at == '?') {
        if (n == 0) ret = solve(text, cs, dp, x-1, 1, c) + solve(text, cs, dp, x-1, 0, c);
        else if (n == cs[c]) ret = solve(text, cs, dp, x-1, 0, c-1);
        else if (n < cs[c]) ret = solve(text, cs, dp, x-1,n+1,c);
        else cout << "bruhhhhh\n";
    }
    else cout << "bruh.\n";
    if (ret == -1) cout << "bruh???\n";

    dp[x][n][c] = ret;
    return ret;
}


int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);

    ifstream in ("inputs/year2023/day12-input.txt");

    string text;
    i64 cn;
    vector<int> cs(100);
    i64 ret {0};
    i64 cases;

    in >> cases;
    for (int j {0}; j < cases; j++) {
        in >> text;
        in >> cn;
        for (int i {1}; i <= cn; i++) {
            in >> cs[i];
        }
        int len = text.size();
        vector<vector<vector<i64>>> dp(len+1, vector<vector<i64>>(len+1, vector<i64>(cn+1, -1)));
        i64 a = solve(text, cs, dp, len, 0, cn);
        ret += a;
    }
    cout << "Sum: " << ret << '\n';
}
