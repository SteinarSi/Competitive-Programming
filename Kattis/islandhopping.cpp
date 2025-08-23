#include <iostream>
#include <iomanip>
#include <set>
#include <queue>
#include <cmath>

using namespace std;
using i64 = int64_t;

double dist(pair<double,double> &a, pair<double,double> &b) {
    return sqrt(pow(abs(a.first - b.first), 2) + pow(abs(a.second - b.second), 2));
}

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);
    cout << setprecision(10) << fixed;

    int n, m, r;
    double x, y, c;
    cin >> n;
    vector<pair<double,double>> points;
    vector<bool> seen;
    for (int i {n}; i --> 0;) {
        cin >> m;
        points = vector<pair<double,double>>(m);
        seen = vector<bool>(m, false);
        for (int j {0}; j < m; j++) {
            cin >> x >> y;
            points[j] = {x, y};
        }

        priority_queue<pair<double, int>> q;
        seen[0] = true;
        r = m - 1;
        c = 0;
        for (int j {1}; j < m; j++) {
            q.push({-dist(points[0], points[j]), j});
        }
        while (r) {
            auto [d, v] = q.top();
            q.pop();
            if ( ! seen[v]) {
                r--;
                c -= d;
                seen[v] = true;
                for (int j {1}; j < m; j++) {
                    if ( ! seen[j]) q.push({-dist(points[v], points[j]), j});
                }
            }
        }
        cout << c << '\n';
    }
}
