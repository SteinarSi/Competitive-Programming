#include <iostream>
#include <vector>
#include <queue>
#include <cmath>

using namespace std;
using i64 = int64_t;


void dijkstra(vector<i64> &dist, vector<vector<pair<i64, int>>> &graph){
    vector<bool> visited(graph.size(), false);
    auto cmp = [](pair<i64, int> a, pair<i64, int> b) -> bool {
        return a.first > b.first;
    };
    //priority_queue<pair<i64, int>, vector<pair<i64, int>>, decltype(cmp)> q;     // alternativt
    priority_queue<pair<i64, int>, vector<pair<i64, int>>, greater<>> q;
    
    visited[1] = true;
    dist[1] = 0;
    for(auto p : graph[1]){
        q.push(p);
        dist[p.second] = p.first;
    } 

    while ( ! q.empty()){
        auto [w, u] = q.top();
        q.pop();
        if (visited[u]) continue;
        visited[u] = true;
        for (auto [w2, v] : graph[u]){
            if (dist[u] + w2 < dist[v]){
                dist[v] = dist[u] + w2;
                q.push({dist[v], v});
            }
        }
    }
}

void dfs(int u, vector<i64> &dist, vector<vector<pair<i64, int>>> &graph, vector<bool> &visited, vector<int> &postorder){
    visited[u] = true;
    for (auto [_, v] : graph[u]) if ( ! visited[v] && dist[v] < dist[u]) dfs(v, dist, graph, visited, postorder);
    postorder.push_back(u);
}

int main(){
    int n, m, a, b;
    i64 w;
    while (cin >> n >> m){
        vector<vector<pair<i64, int>>> graph(n);
        for (int i {0}; i < m; i++){
            cin >> a >> b >> w;
            a--;
            b--;
            graph[a].push_back({w, b});
            graph[b].push_back({w, a});
        }

        vector<i64> dist(n, pow(2, 60));
        vector<int> postorder;
        vector<bool> visited(n, false);
        dijkstra(dist, graph);
        dfs(0, dist, graph, visited, postorder);

        vector<i64> opt(n, 0);
        opt[1] = 1;
        for (int u : postorder){
            if (u == 1) continue;
            for (auto [_, v] : graph[u]){
                if (dist[v] < dist[u]){
                    opt[u] += opt[v];
                }
            }
        }
        cout << opt[0] << endl;
    }
}