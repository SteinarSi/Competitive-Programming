#pragma GCC optimize("O3")

#include <iostream>
#include <iomanip>
#include <string>
#include <vector>

using namespace std;
using u64 = uint64_t;

const u64 A = 911382323;
const u64 B = 972663749;

u64 pow2(u64 base, u64 exp){
    u64 ret = 1;
    while (exp){
        if (exp & 1) ret = (ret * base) % B;
        exp >>= 1;
        base = (base * base) % B;
    }
    return ret;
}

bool equal(const string &s, const int a, const vector<u64> &hash, const vector<u64> &powa){
    int n = s.size();
    return hash[a] * (B + 1 - powa[n]) % B * pow2(B + 1-powa[a], B-2) % B == hash[n];
}

int power_string(const string &s, vector<u64> &hash, vector<int> &bs, const vector<u64> &powa){
    int n = s.size();
    int bc = 0;
    hash[0] = 0;
    for (int i {1}; i <= n; i++) hash[i] = (hash[i-1]*A + s[i-1]) % B;
    for (int a {1}; a*a <= n; a++){
        if (n % a == 0){
            int b = n / a;
            if (a*a != n && a != 1) bs[bc++] = b;
            if (equal(s, a, hash, powa)) return b;
        }
    }

    for (int i {bc-1}; i >= 0; i--){
        int b = bs[i];
        if (equal(s, b, hash, powa)) return n / b;
    }

    return 1;
}

int main(){
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);
    string s;
    int n = 2000001;
    vector<u64> powa(n);
    powa[0] = 1;
    for (int i {1}; i < n; i++) powa[i] = (A * powa[i-1]) % B;
    vector<u64> hash(n);
    vector<int> bs(145);
    while (cin >> s && s != "."){
        cout << power_string(s, hash, bs, powa) << '\n';
    }
}
