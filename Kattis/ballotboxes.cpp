#include <iostream>
#include <iomanip>
#include <queue>

using namespace std;

int main(){
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);
    cout << setprecision(10) << fixed;

    int n, b, x;
    while (cin >> n >> b && (n != -1)) {
        priority_queue<pair<double,pair<int, int>>> q;
        for (int i {n}; i --> 0;) {
            cin >> x;
            q.push({(double) x, {x, 1}});
        }
        b -= n;
        for (; b --> 0 ;) {
            auto [_, xm] = q.top();
            q.pop();
            auto [x, m] = xm;
            q.push({(double) x / (double) (m+1), {x, m+1}});
        }
        auto [_, xm] = q.top();
        auto [x, m] = xm;
        cout << (x+m-1) / m << '\n';
    }
}
