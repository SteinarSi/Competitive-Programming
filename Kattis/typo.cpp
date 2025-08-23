#include <iostream>
#include <vector>
#include <cmath>
#include <string>

using namespace std;
using i64 = int64_t;

i64 mod(i64 a, i64 m){
    i64 r = a % m;
    if (a < 0) return (m + r) % m;
    else return r;
}

i64 hsh(string &s, vector<i64> &p, vector<i64> &h, i64 B){
    h[0] = mod(s[0] * p[s.size()-1], B);
    for (int i {1}; i < s.size(); i++) h[i] = mod(h[i-1] + s[i] * p[s.size()-i-1], B); 
    return h[s.size()-1];
}

i64 hsh_without_r(string &s, vector<i64> &h, i64 A_inv, i64 B, int r){
    return mod(((r==0 ? 0 : h[r-1]) * A_inv) + h[s.size()-1] - h[r], B);
}

bool equal_without_r(string &a, string &s, int r){
    if (a.size() != (s.size()-1)) return false;
    for (int i {0}; i < a.size(); i++) if (a[i] != s[i + (i>=r)]) return false;
    return true;
}

int main(){
    const i64 A = 911382323;
    const i64 B = 972663749;
    const i64 A_inv = 269730666;
    const i64 arr_size = 100000;
    const int H = 1000000;
    vector<i64> p(H);
    p[0] = 1;
    for (int i {1}; i < H; i++) p[i] = mod(p[i-1]*A, B);
    int n;
    cin >> n;
    vector<i64> h(H);
    vector<string> xs(n);
    vector<vector<int>> hashes(arr_size);
    for (int i {0}; i < n; i++){
        cin >> xs[i];
        hashes[mod(hsh(xs[i], p, h, B), arr_size)].push_back(i);
    }
    bool eq;
    bool f = true;
    for (string &s : xs){
        hsh(s, p, h, B);
        eq = false;
        for (int r {0}; r < s.size(); r++){
            for (int &a : hashes[mod(hsh_without_r(s, h, A_inv, B, r), arr_size)]){
                if (equal_without_r(xs[a], s, r)){
                    cout << s << '\n';
                    eq = true;
                    f = false;
                    break;
                }
            }
            if (eq) break;
        }
    }
    if (f) cout << "NO TYPOS\n";
}   