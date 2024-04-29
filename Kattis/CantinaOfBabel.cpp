#include <iostream>
#include <iomanip>
#include <set>
#include <string>
#include <vector>
#include <map>
#include <sstream>
#include <cstdint>
#include <algorithm>

using namespace std;
using i64 = int64_t;

class ID {
    map<string,i64> name2id;
    vector<string> id2name;

public:
    void add(const string &s){
        get_id(s);
    }

    i64 get_id(const string &s){
        auto [it, placed] = name2id.emplace(s, name2id.size());
        if (placed) id2name.push_back(s);
        return it->second;
    }
};

void read_input(int n, vector<vector<int>> &outdegree, vector<vector<int>> &indegree) {
    vector<int> speaks;
    vector<vector<int>> understands(n);
    ID languages;
    string line, name, token;
    for (int i {0}; i < n; i++) {
        cin >> name;

        getline(cin, line);
        stringstream ss(line);

        getline(ss, token, ' ');
        getline(ss, token, ' ');
        int s = languages.get_id(token);
        speaks.push_back(s);
        understands[s].push_back(i);
        while (getline(ss, token, ' ')) {
            int u = languages.get_id(token);
            understands[u].push_back(i);
        }   
        // cout << '\n';
    }

    for (int u {0}; u < n; u++) {
        for (int v : understands[speaks[u]]) {
            if (u == v) continue;
            outdegree[u].push_back(v);
            indegree[v].push_back(u);
        }
    }
}

void visit(int u, vector<vector<int>> &outdegree, vector<bool> &visited, vector<int> &l) {
    if (visited[u]) return;
    visited[u] = true;
    for (int v : outdegree[u]) {
        visit(v, outdegree, visited, l);
    }
    l.push_back(u);
}

void assign(int u, int root, vector<vector<int>> &indegree, vector<int> &comp, vector<int> &size) {
    if (comp[u] != -1) return;
    comp[u] = root;
    size[root]++;
    for (int v : indegree[u]) {
        assign(v, root, indegree, comp, size);
    }
} 

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);
    cout << setprecision(10) << fixed;

    int n;
    cin >> n;
    vector<vector<int>> outdegree(n);
    vector<vector<int>> indegree(n);

    read_input(n, outdegree, indegree);

    vector<int> l;
    vector<bool> visited(n, false);
    for (int u {0}; u < n; u++) {
        visit(u, outdegree, visited, l);
    }
    reverse(l.begin(), l.end());

    vector<int> comp(n, -1);
    vector<int> size(n, 0);
    for (int u : l) {
        assign(u, u, indegree, comp, size);
    }
    
    int best {0};
    for (int s : size) {
        best = max(best, s);
    }
    cout << n - best << '\n';
}
