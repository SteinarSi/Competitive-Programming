#include <iostream>
#include <iomanip>
#include <vector>
#include <map>

using namespace std;

bool nearby(pair<int,int> &u, pair<int,int> &v) {
    auto [x1,y1] = u;
    auto [x2,y2] = v;
    return abs(x1-x2)*abs(x1-x2) + abs(y1-y2)*abs(y1-y2) <= 1000000;
}

bool cross(vector<map<int,pair<int,int>>> &database, int u, int v) {
    for (auto [t, pos] : database[u]) {
        auto start = database[v].lower_bound(t-10);
        for (auto &q = start; q != database[v].end() && q->first <= t+10; q++) {
            if (nearby(pos,q->second)) return true;
        }
    }
    return false;
}

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);

    int p, n, u, x, y, t;
    cin >> p >> n;
    vector<map<int,pair<int,int>>> database(p+1);
    for (int i {0}; i < n; i++) {
        cin >> u >> x >> y >> t;
        database[u].insert({t,{x,y}});
    }

    vector<pair<int,int>> ans;
    for (int u {1}; u <= p; u++) {
        for (int v {u+1}; v <= p; v++) {
            if (cross(database,u,v)) ans.push_back({u,v});
        }
    }
    cout << ans.size() << '\n';
    for (auto [u,v] : ans) {
        cout << u << " " << v << '\n';
    }
}
