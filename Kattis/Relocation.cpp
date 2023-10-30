#include <vector>
#include <iostream>
#include <algorithm>

using namespace std;
using i64 = int64_t;

int main(){
    cin.tie(nullptr);
    ios::sync_with_stdio(false);

    i64 n, q, t, a, b;
    cin >> n >> q;

    vector<i64> map(n+1);
    for (int i {1}; i <= n; cin >> map[i++]);

    for (int i {0}; i < q; i++) {
        cin >> t >> a >> b;
        if (t == 1) map[a] = b;
        else cout << abs(map[a] - map[b]) << endl;
    }
}