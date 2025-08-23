#include <iostream>
#include <string>
#include <iomanip>
#include <vector>
#include <algorithm>

using namespace std;
using i64 = int64_t;

const i64 A = 911382323;
const i64 B = 972663749;

i64 mod(i64 x){
    i64 r = x % B;
    if (x < 0) return (B + r) % B;
    else return r;
}

void rolling_hash(vector<i64> &hash, string &xs) {
    hash[0] = 0;
    for (int i {1}; i <= xs.size(); i++) {
        hash[i] = mod(hash[i-1]*A + xs[i-1]);
    }
}

i64 hash_from_to(vector<i64> &hash, vector<i64> &powa, int a, int b) {
    return mod(hash[b] - hash[a-1] * powa[b-a+1]);
}

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cout << setprecision(10) << fixed;

    string xs;
    cin >> xs;
    i64 n = xs.size();
    string xs2 = xs + xs;

    vector<i64> powa(n);
    powa[0] = 1;
    for (int i {1}; i < n; i++) powa[i] = mod(A * powa[i-1]);

    vector<i64> hash(n*2+1);
    rolling_hash(hash, xs2);

    vector<bool> substring(B,false);
    for (int a {1}; a <= 2*n; a++) {
        for (int b {a+1}; b <= 2*n; b++) {
            substring[hash_from_to(hash, powa, a, b)] = true;
        }
    }
    reverse(xs.begin(), xs.end());
    rolling_hash(hash, xs);

    for (int a {1}; a <= n; a++) {
        for (int b {a+1}; b <= n; b++) {
            if (a == 1 && b == n) continue;
            if ( ! substring[hash_from_to(hash, powa, a, b)]) {
                cout << 0 << '\n';
                return 0;
            }
        }
    }
    cout << 1 << '\n';
}
