#include <iostream>
#include <vector>
#include <iomanip>
#include <queue>

using namespace std;
using i64 = int64_t;

int main(){
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);
    cout << setprecision(10) << fixed;

    priority_queue<pair<i64,pair<i64, i64>>> q;
    i64 h, c, a, d, i;
    cin >> h >> c;
    i64 ret {0};
    for (i = 0; i < c; i++){
        cin >> a >> d;
        ret = max(ret, a);
        q.push({-(a+d), {a, d}});
    }
    for (i = 0; i < h; i++){
        auto [p, r] = q.top();
        auto [a, d] = r;
        q.pop();
        a += d;
        ret = max(ret, a);
        q.push({-(a+d), {a, d}});
    }

    cout << ret << endl;
}