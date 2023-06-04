#include <iostream>
#include <vector>
#include <map>

using namespace std;
using i64 = int64_t;

class UF {
    vector<i64> repr;
    vector<i64> size;

    public:
    UF(i64 n){
        repr = vector<i64>(n, 0);
        size = vector<i64>(n, 1);
        for (i64 i {0}; i < n; i++){
            repr[i] = i;
        }
    }

    i64 find(i64 u){
        if (repr[u] == u) return u;
        i64 r = find(repr[u]);
        repr[u] = r;
        return r;
    }

    void merge(i64 u, i64 v){
        i64 r1 = find(u);
        i64 r2 = find(v);
        if (r1 == r2) return;
        i64 s1 = size[r1];
        i64 s2 = size[r2];
        if (s1 < s2){
            repr[r1] = r2;
            size[r2] += size[r1];
        }
        else{
            repr[r2] = r1;
            size[r1] += size[r2];
        }
    }

    i64 size_of(i64 u){
        return size[find(u)];
    }
};

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

int main(){
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    i64 n;
    cin >> n;
    UF uf = UF(200000);
    ID id = ID();
    string a, b;
    i64 f, t;
    for (i64 i {0}; i < n; i++){
        cin >> a >> b;
        f = id.get_id(a);
        t = id.get_id(b);
        uf.merge(f, t);
        cout << uf.size_of(f) << endl;
    }
}
