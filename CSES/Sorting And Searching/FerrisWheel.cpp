#include <iostream>
#include <vector>
#include <iomanip>
#include <algorithm>

using namespace std;
using u64 = uint64_t;

int main (){
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    u64 n, x, i, j, ret;
    cin >> n >> x;
    vector<u64> xs(n);
    while (cin >> xs[--n]);
    sort(xs.begin(), xs.end());
    for (i=0, j=xs.size()-1; i <= j; i += xs[i] + xs[j--] <= x, ret++);
    cout << ret << '\n';
}