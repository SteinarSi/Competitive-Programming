#include <iomanip>
#include <iostream>
#include <vector>

using namespace std;

int solve(vector<vector<bool>> &seen, vector<pair<int,int>> &xs, int count) {
    if (xs.size() == 0) return count;
    vector<pair<int,int>> next;
    for (auto [x,y] : xs) {
        vector<pair<int,int>> n = {{x-1,y},{x+1,y},{x,y-1},{x,y+1}};
        for (auto [dx,dy] : n) {
            if (dx >= 0 && dx < seen.size() && dy >= 0 && dy < seen[0].size() && !seen[dx][dy]) {
                seen[dx][dy] = true;
                next.push_back({dx,dy});
            }
        }
    }
    return solve(seen, next, count+1);
}

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);

    int r,c,n,x,y;
    cin >> r >> c >> n;
    vector<pair<int,int>> xs;

    for (int i {0}; i < n; i++) {
        cin >> x >> y;
        xs.push_back({x-1,y-1});
    }

    vector<vector<bool>> seen(r, vector<bool>(c, false));
    for (auto [x,y] : xs) {
        seen[x][y] = true;
    }
    cout << solve(seen, xs, 0) << '\n';
}