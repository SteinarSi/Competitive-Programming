#include <iostream>
#include <iomanip>
#include <vector>
#include <queue>

using namespace std;

int main(){
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);
    int n, m, a, b;
    cin >> n >> m;
    vector<vector<int>> graph(n+1);
    for (int i {0}; i < m; i++){
        cin >> a >> b;
        graph[a].push_back(b);
        graph[b].push_back(a);
    }
    vector<int> parent(n+1, -1);
    parent[1] = -2;
    queue<int> q;
    q.push(1);
    while ( ! q.empty() && parent[n] == -1){
        int u = q.front();
        q.pop();
        for (int v : graph[u]){
            if (parent[v] == -1){
                parent[v] = u;
                q.push(v);
                if (v == n) break;
            }
        }
    }
    if (parent[n] == -1){
        cout << "IMPOSSIBLE\n";
        return 0;
    }
    vector<int> path {n};
    while (n != 1){
        n = parent[n];
        path.push_back(n);
    }
    cout << path.size() << '\n';
    for (int i {path.size()-1}; i >= 0; cout << path[i--] << ' ');
    cout << '\n';
}