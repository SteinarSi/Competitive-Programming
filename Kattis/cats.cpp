#include <vector>
#include <iostream>
#include <iomanip>
#include <queue>
#include <queue>

using namespace std;
using i64 = int64_t;

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);

    i64 cases, milk, cats, from, to, dist, cost, thirsty;

    cin >> cases;
    while (cases --> 0) {
        cin >> milk >> cats;
        cost = cats;
        thirsty = cats;
        vector<bool> seen(cats);
        vector<vector<pair<i64,int>>> graph(cats);
        for (int i {0}; i < (cats * (cats-1)) / 2; i++) {
            cin >> from >> to >> dist;
            graph[from].push_back({to, dist});
            graph[to].push_back({from, dist});
        }

        priority_queue<pair<i64,int>> q;
        q.push({0, 0});

        while ( ! q.empty() && thirsty) {
            auto [ negcost, u ] = q.top();
            q.pop();
            if (seen[u]) continue;
            seen[u] = true;
            thirsty--;

            cost -= negcost;
            for (auto [v, d] : graph[u]) {
                if ( ! seen[v]) {
                    q.push({-d, v});
                }
            }
        }

        if (cost <= milk) cout << "yes\n";
        else cout << "no\n";
    }
}