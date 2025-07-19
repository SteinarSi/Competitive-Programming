#include <iostream>
#include <string>
#include <iomanip>
#include <vector>

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

int MAX_LEN = 500000;

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cout << setprecision(10) << fixed;

    vector<i64> powa(MAX_LEN);
    powa[0] = 1;
    for (int i {1}; i < MAX_LEN; i++) powa[i] = mod(A * powa[i-1]);

    vector<i64> hash(MAX_LEN);

    string ps, xs;
    while (getline(cin,ps) && getline(cin,xs)) {
        i64 n = xs.size();
        i64 m = ps.size();
        if (m <= n) {
            rolling_hash(hash, ps);
            i64 target = hash[m];
            rolling_hash(hash, xs);
            for (i64 i {0}; i+m-1 < n; i++) {
                if (mod(hash[i+m] - hash[i] * powa[m]) == target) cout << i << ' ';
            }
        }
        cout << '\n';
    }
}
