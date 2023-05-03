#include <vector>
#include <iostream>
#include <iomanip>
#include <string>

using namespace std;

class UF {
    vector<long long> repr;
    vector<long long> size;
    long long count;

    public:
    UF(long long n){
        repr = vector<long long>(n, 0);
        size = vector<long long>(n, 1);
        count = n;
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
        count--;
    }

    long long size_of(long long u){ return size[find(u)]; }
    long long get_count(){ return count; }
};

int main(){
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);
    int n, m;
    cin >> n >> m;
    UF uf = UF(n*m+1);
    string cur, pre;
    for (int i {0}; i < n; i++){
        cin >> cur;
        for (int j {0}; j < m; j++){
            if (cur[j] == '#') uf.merge(i*m+j, m*n);
            else{
                if (j > 0 && cur[j] == cur[j-1]) uf.merge(i*m+j-1,   i*m+j);
                if (i > 0 && cur[j] == pre[j])   uf.merge((i-1)*m+j, i*m+j);
            } 
        }
        pre = cur;
    }
    cout << uf.get_count() - 1 << endl;
}