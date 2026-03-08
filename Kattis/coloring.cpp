#include <iostream>
#include <vector>
#include <sstream>
#include <algorithm>

using namespace std;

int branch_and_bound(vector<int> &vertices, vector<vector<int>> &graph, vector<vector<bool>> &impossible, int n, int i, int k, int best) {
    if (i == n || k > best) return k;
    int u = vertices[i];

    for (int c {0}; c <= min(n-2, k); c++) {
        if (impossible[u][c]) continue;
        int k2 = k + (c >= k);
        vector<int> revert;
        for (int v : graph[u]) {
            if (!impossible[v][c]) {
                revert.push_back(v);
                impossible[v][c] = true;
            }
        }

        best = min(
            best, 
            branch_and_bound(vertices, graph, impossible, n, i+1, k2, best)
        );

        for (int v : revert) {
            impossible[v][c] = false;
        }
    }

    return best;
}

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);

    int n, v;
    int m {0};
    cin >> n;
    cin.ignore();

    vector<vector<int>> graph(n);
    vector<vector<bool>> impossible(n, vector<bool>(n-1, false));
    vector<int> vertices;
    string line;
    for (int u {0}; u < n; u++) {
        vertices.push_back(u);
        getline(cin, line);
        istringstream iss(line);
        while (iss >> v) {
            m++;
            graph[u].push_back(v);
        }
    }
    if (m == n * (n-1)) {
        cout << n << '\n';
        return 0;
    }
    sort(
        vertices.begin(), 
        vertices.end(), 
        [&](int x, int y) -> bool { 
            return graph[x].size() > graph[y].size();
        }
    );

    for (int v : graph[vertices[0]]) {
        impossible[v][0] = true;
    }

    int k = branch_and_bound(vertices, graph, impossible, n, 1, 1, n);
    cout << k << '\n';
}
