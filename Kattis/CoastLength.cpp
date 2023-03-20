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
    int n, m;
    cin >> n >> m;
    char c;
    n += 2;
    m += 2;
    vector<vector<bool>> graph(n, vector<bool>(m, false));
    for (int y {1}; y < n-1; y++){
        for (int x {1}; x < m-1; x++){
            cin >> c;
            graph[y][x] = c == '1';
        }
    }
    UF uf = UF(n*m);
    for (int y {0}; y < n; y++){
        for (int x {0}; x < m; x++){
            if (y > 0 && graph[y][x] == graph[y-1][x]) uf.merge(y*m+x, (y-1)*m+x);
            if (x > 0 && graph[y][x] == graph[y][x-1]) uf.merge(y*m+x, y*m+x-1);
        }
    }
    
    vector<pair<int, int>> dirs {{-1, 0}, {1, 0}, {0, -1}, {0, 1}};
    long long ret {0};
    for (int y {1}; y < n-1; y++){
        for (int x {1}; x < m-1; x++){
            if ( ! graph[y][x]) continue;
            for (auto [dx, dy] : dirs){
                if (!graph[y+dy][x+dx] && uf.find((y+dy)*m+x+dx) == uf.find(0)) ret += 1;
            }
        }
    }

    cout << ret << endl;
}