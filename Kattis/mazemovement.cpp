#include <iostream>
#include <vector>
#include <algorithm>
#include <queue>
#include <numeric>

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
        bfs_q = vector<int>(edge_count+1);
        Num flow {0};

        do {
            bfs();
            if (seen[sink]) flow += update_flow();
        }
        while(seen[sink]);

        return flow;
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
                if (seen[sink]) break;
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

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);

    i64 n;
    cin >> n;

    i64 s = 0;
    i64 t = 0;

    vector<i64> xs(n);
    for (i64 i {0}; i < n; i++) {
        cin >> xs[i];
        if (xs[i] > xs[t]) t = i;
        if (xs[i] < xs[s]) s = i;
    }

    Flow<i64> flow = Flow<i64>(s,t,n);
    for (i64 u {0}; u < n; u++) {
        for (i64 v {u+1}; v < n; v++) {
            i64 p = gcd(xs[u],xs[v]);
            if (p > 1) {
                flow.add_edge(u, v, p);
                flow.add_edge(v, u, p);
            }
        }
    }

    cout << flow.max_flow_min_cut() << '\n';
}
