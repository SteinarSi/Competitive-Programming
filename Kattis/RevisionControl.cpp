#include <iostream>
#include <iomanip>
#include <vector>

using namespace std;

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);
    cout << setprecision(10) << fixed;

    int n, x;
    vector<bool> seen(1000000001);

    cin >> n;
    for (;n --> 0;) {
        cin >> x;
        cout << !seen[x] << ' ';
        seen[x] = true;
    }
    cout << '\n';
}
