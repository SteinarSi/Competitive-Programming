#include <vector>
#include <iostream>
#include <map>
#include <cmath>

using namespace std;
using i64 = int64_t;

const vector<int> pow2 = {1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384, 32768, 65536, 131072, 262144};
const int lg = 18;

class Tourists {
private:
    vector<i64> depth;
    vector<vector<int>> anc;
    vector<vector<int>> graph;
    vector<int> pre, post;
    int root, n;
    int count = 0;

public:
    Tourists(int _n){
        n = _n;
        root = n/2;
        depth = vector<i64>(n+1);
        depth[root] = 0;
        anc = vector<vector<int>>(n+1, vector<int>(lg+1, -1));
        graph = vector<vector<int>>(n+1);
        pre  = vector<int>(n+1);
        post = vector<int>(n+1);
    }

    void connect(int a, int b){
        graph[a].push_back(b);
        graph[b].push_back(a);
    }

    i64 solve(){
        dna(root, root);
        i64 ret {0};
        for (int u {1}; u <= root; u++) for (int v {u*2}; v <= n; v+=u) ret += lca_dist(u, v);
        return ret;
    }

private:
    void dna(int u, int parent){
        pre[u] = count++;
        depth[u] = depth[parent]+1;
        anc[u][0] = parent;
        for (int k {1}; pow2[k] < depth[u]; k++) anc[u][k] = anc[anc[u][k-1]][k-1];
        for (int v : graph[u]) if (v != parent) dna(v, u);
        post[u] = count++;
    }

    bool is_ancestor(int u, int v){ return pre[u] <= pre[v] && post[u] >= post[v]; }

    i64 lca_dist(int u, int v){
        if (is_ancestor(u, v) || is_ancestor(v, u)) return abs(depth[u] - depth[v]) + 1;
        i64 ret = depth[u] + depth[v];
        for (int i {lg}; i >= 0; i--) if (anc[u][i] != -1 && ! is_ancestor(anc[u][i], v)) u = anc[u][i];
        return ret - 2 * depth[anc[u][0]] + 1;
   }
};

int main(){
    int n, a, b;
    cin >> n;

    Tourists t = Tourists(n);
    for (int i {1}; i < n; i++){
        cin >> a >> b;
        t.connect(a, b);
    }
    cout << t.solve() << '\n';
}