#include <iostream>
#include <iomanip>
#include <vector>

using namespace std;

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);

    int n, a, b, c, d;
    vector<int> seen(251001, 0);
    int it {1};

    while (true) {
        cin >> n;
        if (n == 0) break;

        int ret {0};
        
        for (int i {n}; i --> 0;) {
            cin >> a >> b >> c >> d;
            for (int x {a}; x < c; x++) {
                for (int y {b}; y < d; y++) {
                    ret += seen[501*y + x] != it;
                    seen[501*y + x] = it;
                }
            }
        }

        cout << ret << '\n';
        it++;
    }
}
