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
    vector<u64> q;
    for (int u {1}; u <= n; u++){
        if (indegree[u] == 0) q.push_back(u);
    }
    while ( ! q.empty()){
        a = q.back();
        q.pop_back();
        ret.push_back(a);
        for (int v : graph[a]){
            indegree[v]--;
            if (indegree[v] == 0) q.push_back(v);
        }
    }
    if (ret.size() == n){
        for (int u : ret) cout << u << '\n';
    }else cout << "IMPOSSIBLE\n";
}