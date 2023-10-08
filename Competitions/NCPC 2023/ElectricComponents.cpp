#include <iostream>
#include <string>
#include <vector>
#include <iomanip>
#include <map>
#include <queue>
#include <set>
#include <algorithm>
#include <cstdint>

using namespace std;
using i64 = int64_t;
using u64 = uint64_t;

// TODO: funker ikke, men den besto ganske mange tester før den ble feil da

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);
    cout << setprecision(10) << fixed;

    i64 n, fi, ti;
    cin >> n;
    
    priority_queue<pair<i64, pair<i64,i64>>> q;

    for(i64 i = 0; i < n; i++){
        cin >> fi >> ti;
        q.push({fi*ti, {fi, ti}});
    }

    i64 total = 0;

    while ( ! q.empty()) {
        auto [p1, r1] = q.top();
        q.pop();
        auto [f1, t1] = r1;

        if (q.empty()) {
            total += f1*t1;
            break;
        }

        auto [p2, r2] = q.top();
        q.pop();
        auto [f2, t2] = r2;

        total += max(t1, t2);

        f1--;
        f2--;

        if (f1 > 0) {
            q.push({f1*t1, {f1, t1}});
        }

        if (f2 > 0) {
            q.push({f2*t2, {f2, t2}});
        }
    }

    cout << total << endl;
}
