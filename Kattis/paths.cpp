#include <iostream>
#include <vector>

using namespace std;
using u64 = unsigned long long;

u64 dfs(int u, vector<vector<int>> &opt, vector<vector<int>> &graph, vector<int> &color, int visit_mask){
    if (opt[u][visit_mask] != -1) return opt[u][visit_mask];
    u64 sum {0};
    for (int v : graph[u]){
        if ((1 << color[v]) & visit_mask) continue;
        sum += dfs(v, opt, graph, color, visit_mask | (1 << color[v])) + 1;
    }
    opt[u][visit_mask] = sum;
    return sum;
}

int main(){
    int n, m, k, a, b, c;
    cin >> n >> m >> k;
    vector<int> color(n);
    vector<vector<int>> graph(n);

    for (int i {0}; i < n; i++){
        cin >> c;
        color[i] = c-1;
    }

    for (int i {0}; i < m; i++){
        cin >> a >> b;
        a--; b--;
        graph[a].push_back(b);
        graph[b].push_back(a);
    }

    vector<vector<int>> opt(n, vector<int>(1 << k, -1));

    u64 total {0};
    for (int u {0}; u < n; u++){
        total += dfs(u, opt, graph, color, 1 << color[u]);
    }

    cout << total << endl;
}
