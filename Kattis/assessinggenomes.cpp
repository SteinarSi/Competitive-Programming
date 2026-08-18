#include <iostream>
#include <iomanip>
#include <string>
#include <vector>
#include <cstdint>
#include <algorithm>

using namespace std;
using u64 = uint64_t;

const u64 A = 911382323;
const u64 B = 972663749;
const u64 MAX = 251;

vector<u64> h(MAX);
vector<u64> powa(MAX);

u64 pow2(u64 base, u64 exp){
    u64 ret = 1;
    while (exp){
        if (exp & 1) ret = (ret * base) % B;
        exp >>= 1;
        base = (base * base) % B;
    }
    return ret;
}

bool equal(const string &s, const u64 a){
    u64 n = s.size();
    return h[a] * (B + 1 - powa[n]) % B * pow2(B + 1-powa[a], B-2) % B == h[n];
}

u64 power_string(const string &s){
    u64 n = s.size();
    vector<u64> bs;
    h[0] = 0;
    for (u64 i {1}; i <= n; i++) h[i] = (h[i-1]*A + s[i-1]) % B;
    for (u64 a {1}; a*a <= n; a++){
        if (n % a == 0){
            u64 b = n / a;
            if (a*a != n && a != 1) bs.push_back(b);
            if (equal(s, a)) return a;
        }
    }

    for (int i {(int) bs.size()-1}; i >= 0; i--){
        u64 b = bs[i];
        if (equal(s, b)) return b;
    }

    return n;
}

vector<u64> assess(u64 n) {
    string s;
    vector<u64> ret(n);
    for (u64 i {0}; i < n; i++){
        cin >> s;
        ret[i] = power_string(s);
    }
    sort(ret.begin(), ret.end());
    return ret;
}

int main(){
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);

    powa[0] = 1;
    for (u64 i {1}; i < MAX; i++) powa[i] = (A * powa[i-1]) % B;

    u64 n;
    cin >> n;

    vector<u64> virus = assess(n);
    vector<u64> antivirus = assess(n);

    u64 ret {0};
    for (u64 i {0}; i < n; i++) {
        ret += pow2(virus[i]-antivirus[i], 2);
    }

    cout << ret << '\n';
}
