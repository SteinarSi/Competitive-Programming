#include <iostream>
#include <iomanip>
#include <vector>

using namespace std;

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);

    vector<int> farey(10001);
    farey[0] = 0;
    farey[1] = 2;
    for (int n {0}; n <= 10000; n++) {
        int r = (n * (n+3)) / 2;
        for (int d {2}; d <= n; d++) {
            r -= farey[n / d];
        }
        farey[n] = r;
    }

    int n, i, p;
    cin >> p;
    for (; p --> 0 ;) {
        cin >> i >> n;
        cout << i << ' ' << farey[n] << '\n';
    }
}
