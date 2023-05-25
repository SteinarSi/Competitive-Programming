#include <iostream>
#include <vector>
#include <algorithm>
#include <queue>

using namespace std;

int main(){
    int n, m;
    cin >> n >> m;

    vector<bool> seen(n, false);
    seen[0] = true;

    vector<vector<int>> graph(n);
    vector<int> size;

    int f, t;
    for (int i {0}; i < m; i++){
        cin >> f >> t;
        f--;t--;
        graph[f].push_back(t);
        graph[t].push_back(f);
    }
    int s;
    for (int i {0}; i < n; i++){
        cin >> s;
        size.push_back(s);
    }

    priority_queue<pair<int,int>> q;
    int sum {size[0]};
    for (int v : graph[0]){
        q.push(pair<int, int>(-size[v], v));
        seen[v] = true;
    }
    int v;
    while ( ! q.empty()){
        auto [_, v] = q.top();
        q.pop();
        if (size[v] >= sum) break;
        sum += size[v];
        for (int w : graph[v]){
            if ( ! seen[w]){
                q.push(pair<int, int>(-size[w], w));
                seen[w] = true;
            }
        }
    }

    cout << sum << endl;
}
