#include <iostream>
#include <vector>
#include <map>

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
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    long long n;
    cin >> n;
    UF uf = UF(200000);
    map<string, long long> name_to_id;
    vector<string> id_to_name;
    auto get_id = [&](const string &s) {
        auto [it, placed] = name_to_id.emplace(s, name_to_id.size());
        if (placed) id_to_name.push_back(s);
        return it->second;
    };
    string a, b;
    long long f, t;
    for (long long i {0}; i < n; i++){
        cin >> a >> b;
        f = get_id(a);
        t = get_id(b);
        uf.merge(f, t);
        cout << uf.size_of(f) << endl;
    }
}
