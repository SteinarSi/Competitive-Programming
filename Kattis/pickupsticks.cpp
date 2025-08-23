#include <iostream>
#include <vector>

using namespace std;
using u64 = uint64_t;

int main(){
    u64 n, m, a, b;
    cin >> n >> m;
    vector<vector<u64>> graph(n+1);
    vector<u64> indegree(n+1, 0);
    for (int i {0}; i < m; i++){
        cin >> a >> b;
        graph[a].push_back(b);
        indegree[b]++;
    }

    vector<u64> ret;
    for (int u {1}; u <= n; u++) if ( ! indegree[u]) ret.push_back(u);
    for (int i {0}; i < ret.size(); i++) for (int v : graph[ret[i]]) if (!--indegree[v]) ret.push_back(v);
    
    if (ret.size() == n) for (int u : ret) cout << u << '\n';
    else cout << "IMPOSSIBLE\n";
}