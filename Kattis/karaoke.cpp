#include <iostream>
#include <vector>
#include <iomanip>

using namespace std;

const int INF = 1e9;

int upper_bound(vector<int> &xs, int x) {
    int lo = 0;
    int hi = xs.size();

    while (hi-lo > 1) {
        int mi = (lo+hi) / 2;
        if (x < xs[mi]) hi = mi;
        else lo = mi;
    }

    return hi;
}

int lis(vector<int> &xs) {
    int n = xs.size();
    vector<int> d(n+1, INF);
    d[0] = -INF;

    for (int i = 0; i < n; i++) {
        int l = upper_bound(d, xs[i]);
        if (d[l-1] < xs[i] && xs[i] < d[l]) d[l] = xs[i];
    }

    int ans = 0;
    for (int l = 0; l <= n; l++) {
        if (d[l] < INF) ans = l;
    }
    return ans;
}

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);

    int n, x;
    cin >> n;
    vector<int> xs(n);
    vector<int> ys(n);
    for (int i {0}; i < n; i++) {
        cin >> x;
        xs[i] = x;
        ys[n-i-1] = x;
    }

    cout << max(lis(xs), lis(ys)) << '\n';
}
