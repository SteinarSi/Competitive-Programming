#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

int cost(int u, int parent, vector<vector<pair<int, int>>> &graph){
    if (u != parent && graph[u].size() == 1) return 99999999;

    int sum {0};
    for (auto [v, w] : graph[u]){
        if (v == parent) continue;
        sum += min(w, cost(v, u, graph));
    }
    return sum;
}

int main(){
    int n, r;
    int a, b, w;
    while (cin >> n >> r){
        vector<vector<pair<int, int>>> graph(n);
        for (int i {0}; i < n-1; i++){
            cin >> a >> b >> w;
            a--;
            b--;
            graph[a].push_back(pair(b, w));
            graph[b].push_back(pair(a, w));
        }
        cout << cost(r-1, r-1, graph) << endl;
    }
}