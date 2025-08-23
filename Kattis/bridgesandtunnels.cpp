#include <iostream>
#include <vector>
#include <map>

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
    UF uf = UF<i64>(200000);
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
