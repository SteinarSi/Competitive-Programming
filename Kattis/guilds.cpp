#include <iostream>
#include <set>
#include <vector>
#include <string>
#include <map>

using namespace std;

void uf(string u, map<string, string> &representative, map<string, vector<string>> &graph, string replacement){
    for (string v : graph[u]){
        representative[v] = replacement;
        uf(v, representative, graph, replacement);
    }
}

int main(){
    int n;
    cin >> n;

    map<string, string> representative;
    map<string, vector<string>> graph;
    set<string> not_sinks;
    vector<string> all;

    string inn;
    string slave;
    string master;
    int pos;
    for (int i {0}; i < n; i++){
        cin >> slave >> master;
        all.push_back(slave);

        if (graph.count(master) == 0){
            vector<string> slaves;
            graph.insert(pair(master, slaves));
        }
        graph[master].push_back(slave);

        not_sinks.insert(slave);
    }

    for (auto [u, _] : graph){
        if (not_sinks.count(u) > 0) continue;
        uf(u, representative, graph, u);
    }

    for (string u : all){
        cout << u << ' ' << representative[u] << endl;
    }
}