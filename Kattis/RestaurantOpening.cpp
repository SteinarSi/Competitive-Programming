#include <iostream>
#include <iomanip>
#include <vector>
#include <algorithm>

using namespace std;
using u64 = uint64_t;

u64 total_dist(int x, int y, int n, int m, vector<vector<u64>> &map){
    u64 ret {0};
    for (int u {0}; u < n; u++){
        for (int v {0}; v < m; v++){
            ret += map[u][v] * (abs(x-v) + abs(y-u));
        }
    }
    return ret;
}

int main(){
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    int n, m, y, x;
    cin >> n >> m;
    vector<vector<u64>> map(n, vector<u64>(m));
    for (y=0; y<n; y++){
        for (x=0; x<m; cin >> map[y][x++]);
    }
    u64 best = 99999999999999;
    for (y=0; y<n; y++){
        for (x=0; x<m; x++){
            best = min(best, total_dist(x, y, n, m, map));
        }
    }

    cout << best << '\n';
}