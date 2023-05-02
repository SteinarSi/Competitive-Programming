#include <iostream>
#include <vector>
#include <algorithm>
#include <queue>
#include <numeric>
#include <string>

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

int main(){
    int n, m, a, b;
    cin >> n >> m >> a >> b;

    vector<string> land(n);
    for (int i {0}; i < n; cin >> land[i++]){}
    
    int s {n*m};
    int t {n*m+1};
    Flow<i64> flow = Flow<i64>(s, t, n*m+2);
    int u, v;
    for (int y {0}; y < n; y++){
        for (int x {0}; x < m; x++){
            u = y*m + x;
            if (y > 0) {
                v = (y-1)*m + x;
                flow.add_edge(u, v, a);
                flow.add_edge(v, u, a);
            }
            if (x > 0){
                v = y*m + x-1;
                flow.add_edge(u, v, a);
                flow.add_edge(v, u, a);
            }

            if (land[y][x] == '.') flow.add_edge(s, u, b);
            else flow.add_edge(u, t, b);
        }
    }

    cout << flow.max_flow_min_cut() << '\n';
}