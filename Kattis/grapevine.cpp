#include <iostream>
#include <iomanip>
#include <string>
#include <map>
#include <vector>
#include <queue>

using namespace std;

class ID {
    map<string,int> name2id;
    vector<string> id2name;

public:
    void add(const string &s){
        get_id(s);
    }

    int get_id(const string &s){
        auto [it, placed] = name2id.emplace(s, name2id.size());
        if (placed) id2name.push_back(s);
        return it->second;
    }
};

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);

    int n,m,d,s;
    string name1, name2;
    ID id = ID();

    cin >> n >> m >> d;
    vector<int> skepticism(n);
    for (int i {0}; i < n; i++) {
        cin >> name1 >> s;
        skepticism[id.get_id(name1)] = s;
    }
    vector<vector<int>> graph(n);
    for (int i {0}; i < m; i++) {
        cin >> name1 >> name2;
        int u = id.get_id(name1);
        int v = id.get_id(name2);
        graph[u].push_back(v);
        graph[v].push_back(u);
    }

    cin >> name1;
    int u = id.get_id(name1);
    vector<bool> convinced(n, false);
    vector<bool> heard(n, false);
    convinced[u] = true;
    heard[u] = true;
    int ret = 0;

    vector<int> q = { u };
    while (d --> 0 && q.size()) {
        vector<int> next;
        for (int u : q) {
            for (int v : graph[u]) {
                if ( ! heard[v]) {
                    ret++;
                    heard[v] = true;
                }
                if ( ! convinced[v] && --skepticism[v] <= 0) {
                    convinced[v] = true;
                    next.push_back(v);
                }
            }
        }
        q = next;
    }

    cout << ret << '\n';
}
