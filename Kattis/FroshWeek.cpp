#include <iostream>
#include <iomanip>
#include <vector>
#include <cstdint>

using namespace std;
using u64 = uint64_t;

pair<u64,vector<u64>> sort_and_count(vector<u64> &xs, u64 start, u64 end) {
    vector<u64> ret;
    if (start == end) return {0, ret};
    if (start + 1 == end) {
        ret.push_back(xs[start]);
        return {0, ret};
    }
    u64 mid = (start + end) / 2;
    auto [c1, l1] = sort_and_count(xs, start, mid);
    auto [c2, l2] = sort_and_count(xs, mid, end);

    u64 p1 = 0;
    u64 p2 = 0;
    u64 c = c1 + c2;

    while (p1 + start < mid && p2 + mid < end) {
        if (l1[p1] <= l2[p2]) {
            ret.push_back(l1[p1++]);
        }
        else {
            ret.push_back(l2[p2++]);
            c += l1.size() - p1; // <- this is where the magic happens - Pål, 2022
        }
    }
    for (; p1 < l1.size(); ret.push_back(l1[p1++]));
    for (; p2 < l2.size(); ret.push_back(l2[p2++]));

    return {c, ret};
}

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);
    cout << setprecision(10) << fixed;

    u64 n;
    cin >> n;
    vector<u64> xs(n);
    for (u64 i {0}; i < n; cin >> xs[i++]);

    auto [c, _] = sort_and_count(xs, 0, n);
    cout << c << '\n';
}
