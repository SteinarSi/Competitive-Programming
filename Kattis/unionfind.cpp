#include <vector>
#include <iostream>
#include <iomanip>

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

    i64 n, q, a, b;
    char o;
    cin >> n >> q;

    UF uf = UF(n);
    for (int i {0}; i < q; i++) {
        cin >> o >> a >> b;
        if (o == '?') {
            if (uf.find(a) == uf.find(b)) cout << "yes\n";
            else cout << "no\n";
        }
        else uf.merge(a, b);
    }
}