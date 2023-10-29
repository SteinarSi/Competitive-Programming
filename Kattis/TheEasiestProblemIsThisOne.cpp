#include <iostream>
#include <iomanip>

using namespace std;

int digit_sum(int p) {
    if (p == 0) return 0;
    return p % 10 + digit_sum(p / 10);
}

int solve(int p) {
    int s = digit_sum(p);
    int q = 10;
    while (q++) {
        if (digit_sum(p * q) == s) return q;
    }
}

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);
    int p;
    while (cin >> p && p > 0){
        cout << solve(p) << '\n';
    }
}