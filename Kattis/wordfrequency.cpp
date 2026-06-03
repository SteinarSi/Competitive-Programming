#include <string>
#include <map>
#include <queue>
#include <iomanip>
#include <iostream>

using namespace std;

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);

    int n, k, m;
    string x;
    cin >> n >> k >> m;

    map<string,int> seen;
    for (;n --> 0;) {
        cin >> x;
        if (x.size() >= m) {
            if (seen.find(x) != seen.end()) seen[x] += 1;
            else seen[x] = 1;
        }
    }

    priority_queue<pair<int,string>, vector<pair<int,string>>, std::greater<pair<int,string>>> queue;
    for (auto const& [x,s] : seen) {
        queue.push({-s,x});
    }

    for (;k --> 0;) {
        auto [s,x] = queue.top();
        queue.pop();
        cout << x << ' ' << -s << '\n';
    }
}
