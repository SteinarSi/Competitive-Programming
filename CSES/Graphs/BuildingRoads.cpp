#include <vector>
#include <iostream>
#include <iomanip>

using namespace std;
using u64 = uint64_t;

class UF {
    vector<u64> repr;
    vector<u64> size;
    u64 count;

    public:
    UF(u64 n){
        count = n;
        repr = vector<u64>(n, 0);
        size = vector<u64>(n, 1);
        for (u64 i {0}; i < n; i++){
            repr[i] = i;
        }
    }

    u64 find(u64 u){
        if (repr[u] == u) return u;
        u64 r = find(repr[u]);
        repr[u] = r;
        return r;
    }

    void merge(u64 u, u64 v){
        u64 r1 = find(u);
        u64 r2 = find(v);
        if (r1 == r2) return;
        u64 s1 = size[r1];
        u64 s2 = size[r2];
        if (s1 < s2){
            repr[r1] = r2;
            size[r2] += size[r1];
        }
        else{
            repr[r2] = r1;
            size[r1] += size[r2];
        }
        count--;
    }

    u64 components() { return count; }
};

int main(){
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);
    int n, m, a, b;
    cin >> n >> m;
    UF uf = UF(n);
    for (int i {0}; i < m; i++){
        cin >> a >> b;
        uf.merge(a-1, b-1);
    }
    cout << uf.components()-1 << '\n';

    for (int i {0}; i < n; i++){
        if (uf.find(0) != uf.find(i)){
            cout << 1 << ' ' << i+1 << '\n';
            uf.merge(0, i);
        }
    }
}