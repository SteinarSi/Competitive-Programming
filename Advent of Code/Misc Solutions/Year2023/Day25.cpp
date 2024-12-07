// TODO: need to update the parser


#include <iostream>
#include <vector>
#include <algorithm>
#include <queue>
#include <string>
#include <map>
#include <iomanip>
#include <fstream>

using namespace std;
using i64 = int64_t;

template<typename Num>
class Flow {
    private:
    int size;
    int source;
    int sink;
    int edge_count = 0;
    vector<int> to;
    vector<Num> cap;
    vector<vector<int>> edge_list;
    vector<int> parent;
    vector<int> bfs_q;

    public:
    vector<bool> seen;
    Flow(int _source, int _sink, int n){
        source = _source;
        sink = _sink;
        size = n;
        edge_list = vector<vector<int>>(size);
    };

    void add_edge(int u, int v, Num weight){
        to.push_back(v); // add forwards edge
        cap.push_back(weight);
        edge_list[u].push_back(edge_count++);
        to.push_back(u); // add backwards residual edge
        cap.push_back(0);
        edge_list[v].push_back(edge_count++);
    }

    void add_vertex_costs(vector<Num> &costs){
        edge_list = vector<vector<int>>(size*2);
        int old_count = edge_count;
        edge_count = 0;
        for (int edge {0}; edge < old_count; edge+=2){
            edge_list[size+to[edge+1]].push_back(edge_count++);
            edge_list[to[edge]].push_back(edge_count++);
            to[edge+1] = size+to[edge+1];
        }
        for (int u {0}; u < size; u++){
            add_edge(u, size+u, costs[u]);
        }

        source += size;
        size *= 2;
    }

    Num max_flow_min_cut(){
        parent = vector<int>(size);
        seen = vector<bool>(size);
        bfs_q = vector<int>(edge_count);
        Num flow {0};
        do {
            bfs();
            if (seen[sink]) flow += update_flow();
        }
        while(seen[sink]);

        // printf("Flow from %d to %d is ", source, sink);

        if (flow == 3) {
            int reachable = 0;
            int unreachable = 0;
            for (int i {0}; i < size; i++) {
                if (seen[i]) reachable++;
                else unreachable++;
            }

            printf("Three! ");
            printf("Component sizes are %d * %d = %d.\n", reachable, unreachable, reachable * unreachable);
            return true;
        }
        // else printf("%ld.\n", flow);

        // return flow;
        return false;
    }

    private:
    Num update_flow(){
        int v = sink;
        Num bottleneck = cap[parent[v]];
        while (v != source){
            bottleneck = min(bottleneck, cap[parent[v]]);
            v = to[parent[v]^1];
        }
        v = sink;
        while (v != source){
            cap[parent[v]] -= bottleneck;
            cap[parent[v]^1] += bottleneck;
            v = to[parent[v]^1];
        }
        return bottleneck;
    }

    void bfs(){
        bfs_q[0] = source;
        int next = 0;
        int count = 1;
        seen.assign(size, false);
        seen[source] = true;
        while (next < count) {
            int u = bfs_q[next++];
            for (int edge : edge_list[u]){
                if (seen[to[edge]] || cap[edge] <= 0) continue;
                parent[to[edge]] = edge;
                bfs_q[count++] = to[edge];
                seen[to[edge]] = true;
                // if (seen[sink]) break;
            }
            if (seen[sink]) break;
        }
    }
};

template<typename Num>
Flow<Num> from_adj_list(int s, int t, vector<vector<pair<int,Num>>> &g){
    Flow<Num> ret = Flow<Num>(s, t, g.size());
    for (int u {0}; u < g.size(); u++){
        for (auto [v, w] : g[u]){
            ret.add_edge(u, v, w);
        }
    }
    return ret;
}

template<typename Num>
Flow<Num> from_adj_matrix(int s, int t, vector<vector<Num>> &g){
    Flow<Num> ret = Flow<Num>(s, t, g.size());
    int edges {0};
    for (int u {0}; u < g.size(); u++){
        for (int v {0}; v < g.size(); v++){
            if (g[u][v] > 0) ret.add_edge(u, v, g[u][v]);
        }
    }
    return ret;
}

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


int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);

    ifstream in ("inputs/year2023/day25-input.txt");
    int n, q, u, v, m;
    in >> n >> q;
    string uu, vv;
    vector<vector<pair<int,i64>>> graph(n);
    ID idd = ID();

    for (int i {0}; i < q; i++) {
        in >> uu;
        u = idd.get_id(uu);
        in >> m;
        for (int j {0}; j < m; j++) {
            in >> vv;
            v = idd.get_id(vv);
            graph[u].push_back({v,1});
            graph[v].push_back({u,1});
        }
    }

    for (int source {0}; source < n; source++) {
        printf("Starting from %d\n", source);
        for (int sink {source+1}; sink < n; sink++ ) {
            Flow flow = from_adj_list(source, sink, graph);
            if (flow.max_flow_min_cut()) return 0;
        }
    }
}
