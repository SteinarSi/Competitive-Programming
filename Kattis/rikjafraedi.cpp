#pragma GCC optimize("O3")

#include <iostream>
#include <iomanip>
#include <vector>

using namespace std;
using i64 = uint64_t;

struct Arrow {
    i64 dx;
    i64 dy;
    i64 dz;

    Arrow add(Arrow &other) const {
        return Arrow{
            dx+other.dx,
            dy+other.dy,
            dz+other.dz,
        };
    }

    bool eq(Arrow other) const {
        return dx == other.dx && dy == other.dy && dz == other.dz;
    }
};

const Arrow ID = Arrow{0,0,0};

bool dfs(vector<vector<pair<i64,Arrow>>> &graph, vector<Arrow> &value, vector<bool> &seen, i64 u) {
    seen[u] = true;
    for (auto [v, arr] : graph[u]) {
        if (u == v) {
            if ( ! ID.eq(arr)) return true;
        }
        else if ( ! seen[v]) {
            value[v] = value[u].add(arr);
            if (dfs(graph, value, seen, v)) return true;
        }
        else if ( ! value[v].eq(value[u].add(arr))) return true;
    }
    return false;
}

int main(){
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);

    i64 n,m,a,b,x,y,z;
    cin >> n >> m;
    vector<vector<pair<i64,Arrow>>> graph(n);

    for (int i {0}; i < m; i++) {
        cin >> a >> b >> x >> y >> z;
        a--;
        b--;
        graph[a].push_back({b, Arrow{x,y,z}});
        graph[b].push_back({a, Arrow{-x,-y,-z}});
    }

    vector<Arrow> value(n);
    vector<bool> seen(n, false);

    for (i64 u {0}; u < n; u++) {
        if (seen[u]) continue;
        value[u] = ID;
        if (dfs(graph, value, seen, u)){
            cout << "Neibb" << endl;
            return 0;
        }
    }
    cout << "Jebb" << endl;
}
