#include <iostream>
#include <iomanip>
#include <vector>

using namespace std;

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);
    cout << setprecision(10) << fixed;

    int n;
    cin >> n;
    vector<int> xs(n);
    int total {0};
    for (int i {0}; i < n; i++) {
        cin >> xs[i];
        total += xs[i];
    }
    if (total % 3 != 0) {
        cout << "-1\n";
        return 0;
    }
    int size {total / 3};
    int curr {0};
    for (int i {0}; i < n-1; i++) {
        curr += xs[i];
        if (curr > size) {
            cout << "-1\n";
            return 0;
        }
        if (curr == size) {
            for (int j {i+1}; j < n; j++) {
                curr += xs[j];
                if (curr > 2 * size) {
                    cout << "-1\n";
                    return 0;
                }
                if (curr == 2 * size) {
                    cout << i+1 << ' ' << j + 1 << '\n';
                    return 0;
                }
            }
            cout << "-1\n";
            return 0;
        }
    }
}
