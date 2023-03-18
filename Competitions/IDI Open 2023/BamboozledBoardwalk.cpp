
#include <iostream>
#include <vector>
#include <map>
#include <iomanip>
#include <algorithm>

using namespace std;
using i64 = int64_t;

class UF {
    vector<i64> repr;
    vector<i64> size;

    public:
    UF(i64 n){
        repr = vector<i64>(n, 0);
        size = vector<i64>(n, 1);
        for (i64 i {0}; i < n; i++){
            repr[i] = i;
        }
    }

    i64 find(i64 u){
        if (repr[u] == u) return u;
        i64 r = find(repr[u]);
        repr[u] = r;
        return r;
    }

    void merge(i64 u, i64 v){
        i64 r1 = find(u);
        i64 r2 = find(v);
        if (r1 == r2) return;
        i64 s1 = size[r1];
        i64 s2 = size[r2];
        if (s1 < s2){
            repr[r1] = r2;
            size[r2] += size[r1];
        }
        else{
            repr[r2] = r1;
            size[r1] += size[r2];
        }
    }

    i64 size_of(i64 u){
        return size[find(u)];
    }
};

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);
    
    i64 n, m;
    cin >> n >> m;
    vector<pair<i64, i64>> bridges(m);
    vector<pair<i64, i64>> costs(m);
    i64 u, v, c;
    for (i64 i {0}; i < m; i++){
        cin >> u >> v >> c;
        bridges[i] = {u, v};
        costs[i] = {c, i};
    }
    sort(costs.begin(), costs.end());
    i64 cost = 0;
    vector<i64> opt(m, false);
    UF uf = UF(n);
    for (i64 i {0}; i < m; i++){
        auto [u, v] = bridges[costs[i].second];
        if (uf.find(u) == uf.find(v)) continue;
        uf.merge(u, v);
        opt[i] = true;
        cost += costs[i].first;
    }
    
    i64 best = cost;
    for (i64 i {0}; i < m; i++){
        if ( ! opt[i]) continue;
        uf = UF(n);
        cost = 0;
        for (i64 j {0}; j < m; j++){
            if (j == i) continue;
            auto [u, v] = bridges[costs[j].second];
            if (uf.find(u) == uf.find(v)) continue;
            uf.merge(u, v);
            cost += costs[j].first;
        }
        if (uf.size_of(0) == n && cost > best){
            best = cost;
        }
    }

    cout << best << endl;
}
