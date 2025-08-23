#include <iostream>
#include <iomanip>
#include <vector>

using namespace std;

pair<vector<int>,int> greedy(vector<int> &coins, int target) {
    vector<int> g(coins.size(), 0);
    int t {0};

    for (int i {0}; target > 0; i++) {
        int q = target / coins[i];
        g[i] += q;
        t += q;
        target -= q * coins[i];
    }

    return {g, t};
}

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);
    cout << setprecision(10) << fixed;

    int n;
    cin >> n;
    vector<int> coins(n);
    for (int i {n}; i --> 0; cin >> coins[i]);

    for (int i {1}; i < n; i++) {
        vector<int> g = greedy(coins, coins[i-1]-1).first;
        int w {0};
        int used {0};
        for (int k {0}; k < i; k++) {
            w += g[k] * coins[k];
            used += g[k];
        }
        for (int j {i}; j < n; j++) {
            w += g[j] * coins[j];
            used += g[j];
            if (greedy(coins, w + coins[j]).second > used+1) {
                cout << "non-canonical\n";
                return 0;
            }
            
        }
    }

    cout << "canonical\n";
}
