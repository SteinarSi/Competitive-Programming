#include <iostream>
#include <iomanip>
#include <string>

using namespace std;
using i64 = int64_t;

const i64 M = 1000000007;

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);

    string seq;
    cin >> seq;
    
    i64 k2 = 1;
    i64 num1 = 0;
    i64 ret = 0;
    for (char c : seq) {
        if (c == '0') {
            ret = (ret + num1) % M;
        }
        else if (c == '1') {
            num1 = (num1 + k2) % M;
        }
        else {
            ret = ((ret << 1) + num1) % M;
            num1 = ((num1 << 1) + k2) % M;
            k2 = (k2 << 1) % M;
        }
    }

    cout << ret << '\n';
}
