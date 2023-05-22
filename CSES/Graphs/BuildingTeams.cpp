#include <iostream>
#include <iomanip>
#include <vector>

using namespace std;

bool color(int u, vector<int> &colors, int c, vector<vector<int>> &graph){
    if (colors[u] == c) return false;
    if (colors[u] == 1-c) return true;
    colors[u] = c;
    for (int v : graph[u]){
        if (color(v, colors, 1-c, graph)) return true;
    }
    return false;
}

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
    vector<int> colors(n+1, -1);
    for (int u {1}; u <= n; u++){
        if (colors[u] == -1 && color(u, colors, 0, graph)){
            cout << "IMPOSSIBLE\n";
            return 0; 
        }
    }
    for (int u {1}; u <= n; cout << colors[u++]+1 << ' ');
    cout << '\n';
}