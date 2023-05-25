#include <iostream>
#include <iomanip>
#include <vector>
#include <algorithm>

using namespace std;

int main(){
    int n;
    vector<vector<int>> graph;
    int a;
    while (cin >> n && n != -1){
        vector<vector<int>> graph(n);
        for (int u {0}; u < n; u++){
            for (int v {0}; v < n; v++){
                cin >> a;
                if (a){
                    graph[u].push_back(v);
                    graph[v].push_back(u);
                } 
            }
        }
        for (int u {0}; u < n; u++){
            bool weak = true;
            for (int v : graph[u]){
                for (int w : graph[v]){
                    if (find(graph[u].begin(), graph[u].end(), w) != graph[u].end()){
                        weak = false;
                        break;
                    }
                }
                if ( ! weak) break;
            }
            if (weak) cout << u << ' ';
        }
        cout << '\n';
    }
}