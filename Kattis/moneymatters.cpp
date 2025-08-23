#include <vector>
#include <iostream>

using namespace std;

class UF {
    vector<long long> repr;
    vector<long long> size;

    public:
    UF(long long n){
        repr = vector<long long>(n, 0);
        size = vector<long long>(n, 1);
        for (long long i {0}; i < n; i++){
            repr[i] = i;
        }
    }

    long long find(long long u){
        if (repr[u] == u) return u;
        long long r = find(repr[u]);
        repr[u] = r;
        return r;
    }

    void merge(long long u, long long v){
        long long r1 = find(u);
        long long r2 = find(v);
        if (r1 == r2) return;
        long long s1 = size[r1];
        long long s2 = size[r2];
        if (s1 < s2){
            repr[r1] = r2;
            size[r2] += size[r1];
        }
        else{
            repr[r2] = r1;
            size[r1] += size[r2];
        }
    }

    long long size_of(long long u){
        return size[find(u)];
    }
};

int main(){
    int n, f;
    cin >> n >> f;
    vector<long long> money(n, 0);
    int m;
    for (int i {0}; i < n; i++){
        cin >> m;
        money[i] = m;
    }
    UF uf = UF(n);
    int a, b;
    for (int i {0}; i < f; i++){
        cin >> a >> b;
        uf.merge(a, b);
    }

    for (int i {0}; i < n; i++){
        m = money[i];
        money[uf.find(i)] += m;
        money[i] -= m;
    }

    bool possible = true;
    for (int i {0}; i < n; i++){
        if (money[i] != 0) possible = false;
    }
    if (possible) cout << "POSSIBLE\n";
    else cout << "IMPOSSIBLE\n";
}