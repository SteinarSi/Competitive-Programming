#include <iomanip>
#include <iostream>
#include <vector>
#include <queue>

using namespace std;
using i64 = int64_t;

i64 solve(vector<i64> &xs) {
    priority_queue<i64> lower;
    priority_queue<i64, vector<i64>, greater<i64>> higher;

    i64 ret = xs[0];
    lower.push(xs[0]);

    for (long unsigned int i {1}; i < xs.size(); i++) {
        i64 x = xs[i];
        if (x <= lower.top()) {
            lower.push(x);
            if (lower.size() > higher.size()+1) {
                higher.push(lower.top());
                lower.pop();
            }
        }
        else {
            higher.push(x);
            if (higher.size() > lower.size()) {
                lower.push(higher.top());
                higher.pop();
            }
        }
        
        if (i & 1) ret += (lower.top() + higher.top()) >> 1;
        else ret += lower.top();
    }

    return ret;
}

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);

    i64 t, n;
    cin >> n;
    while (n --> 0) {
        cin >> t;
        vector<i64> xs(t);
        for (i64 i {0}; i < t; cin >> xs[i++]);
        cout << solve(xs) << '\n';
    }
}
