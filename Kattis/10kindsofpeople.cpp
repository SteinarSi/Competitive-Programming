#include <vector>
#include <string>
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
    int r, c;
    cin >> r >> c;
    
    vector<vector<bool>> decimal(r, vector<bool>(c, false));
    UF uf = UF(c * r);

    string prev, curr;
    for (int i {0}; i < r; i++){
        cin >> curr;
        for (int j {0}; j < c; j++){
            if (j > 0 && curr[j] == curr[j-1]){
                uf.merge(i * c + j, i * c + j -1);
            }
            if (i > 0 && curr[j] == prev[j]){
                uf.merge(i * c + j, (i-1) * c + j);
            }
            if (curr[j] == '1') decimal[i][j] = true;
        }
        prev = curr;
    }
    int m, r1, c1, r2, c2;
    cin >> m;
    for (int i {0}; i < m; i++){
        cin >> r1 >> c1 >> r2 >> c2;
        r1--;c1--;r2--;c2--;
        if (uf.find(r1 * c + c1) != uf.find(r2 * c + c2)) cout << "neither\n";
        else if (decimal[r1][c1]) cout << "decimal\n";
        else cout << "binary\n";
    }
}