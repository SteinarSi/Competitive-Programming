#include <vector>
#include <iomanip>
#include <iostream>
#include <cstdint>
#include <queue>

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

    i64 n, c;
    cin >> n;
    priority_queue<pair<i64,pair<i64,i64>>> q;

    for (i64 a {0}; a < n; a++) {
        for (i64 b {0}; b < n; b++) {
            cin >> c;
            if (a < b) {
                q.push({-c, {a,b}});
            }
        }
    }

    UF uf = UF(n);
    while ( ! q.empty()) {
        auto [c, ab] = q.top();
        auto [a, b] = ab;
        q.pop();
        if (uf.find(a) != uf.find(b)) {
            cout << a+1 << ' ' << b+1 << endl;
            uf.merge(a,b);
        }
        if (uf.components() == n) break;
    }
}
