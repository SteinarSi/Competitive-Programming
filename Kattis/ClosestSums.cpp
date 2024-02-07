#pragma GCC optimize("O3")

#include <iostream>
#include <vector>

using namespace std;
using i64 = int64_t;

int main(){
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    
    i64 n, m, q, b;
    i64 its {1};
    while (cin >> n) {
        cout << "Case " << its++ << ":\n";
        vector<i64> xs(n);
        for (int i {0}; i < n; cin >> xs[i++]);

        cin >> m;
        for (i64 c {0}; c < m; c++) {
            cin >> q;
            b = 99999999999999;
            for (int i {0}; i < n; i++) {
                for (int j {i+1}; j < n; j++) {
                    if (abs(xs[i] + xs[j] - q) < abs(b - q)) b = xs[i] + xs[j];
                }
            }
            cout << "Closest sum to " << q << " is " << b << ".\n";
        }
    }
}