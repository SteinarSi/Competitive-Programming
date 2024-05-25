#include <iostream>
#include <iomanip>
#include <algorithm>
#include <vector>

using namespace std;

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);
    cout << setprecision(10) << fixed;

    int k, q, p;
    cin >> k >> q;

    vector<int> count(k+1, 0);
    for (int i {q}; i --> 0;) {
        cin >> p >> p;
        count[p]++;
    }

    int b = 99999999;
    for (int i {1}; i <= k; b = min(b, count[i++]));

    cout << b << '\n';
}
