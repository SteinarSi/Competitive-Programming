#include <iostream>
#include <vector>
#include <iomanip>
#include <cstdint>
#include <algorithm>

using namespace std;
using i64 = int64_t;

template <typename Num>
class UF {
    vector<Num> repr;
    vector<Num> size;
    Num comps;

    public:
    UF(Num n){
        repr = vector<Num>(n, 0);
        size = vector<Num>(n, 1);
        comps = n;
        for (Num i {0}; i < n; repr[i++] = i);
    }
    Num find(Num u){
        if (repr[u] == u) return u;
        Num r = find(repr[u]);
        repr[u] = r;
        return r;
    }
    void merge(Num u, Num v){
        Num r1 = find(u);
        Num r2 = find(v);
        if (r1 == r2) return;
        Num s1 = size[r1];
        Num s2 = size[r2];
        if (s1 < s2){
            repr[r1] = r2;
            size[r2] += size[r1];
        }
        else{
            repr[r2] = r1;
            size[r1] += size[r2];
        }
        comps--;
    }
    Num size_of(Num u){ return size[find(u)]; }
    Num components(){ return comps; }
};

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);
    cout << setprecision(10) << fixed;

    i64 n, u, v;
    cin >> n;
    vector<pair<i64,i64>> xs(n);
    for (int i {0}; i < n; i++) {
        cin >> u >> v;
        xs[i] = {u,v};
    }
    vector<pair<i64,pair<int,int>>> edges;
    for (int i {0}; i < n; i++) {
        auto [x,y] = xs[i];
        for (int j {i+1}; j < n; j++) {
            auto [p,q] = xs[j];
            edges.push_back({abs(x-p) + abs(y-q), {i,j}});
        }
    }
    sort(edges.begin(), edges.end());

    UF uf = UF<int>(n);
    i64 ret = 0;

    for (auto [c, ij] : edges) {
        auto [i,j] = ij;
        if (uf.find(i) == uf.find(j)) continue;
        ret += c * 2;
        uf.merge(i,j);
        if (uf.components() <= 1) break;
    }

    cout << ret << '\n';
}
