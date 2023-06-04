#include <iostream>
#include <string>
#include <vector>
#include <iomanip>
#include <algorithm>
#include <queue>
#include <set>


using namespace std;
using i64 = int64_t;

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);

    int t, n;
    cin >> t;
    i64 a, b;
    for (int caze {0}; caze < t; caze++){
        cin >> n;
        vector<pair<pair<i64,i64>,int>> q;
        vector<bool> broken(n+1, false);
        vector<bool> on(n+1, false);
        vector<vector<int>> indices(n+1);
        for (int i {0}; i < n; i++){
            cin >> a >> b;
            q.push_back({{a, -b}, i});
            indices[a].push_back(i);
        }

        sort(q.begin(), q.end());
        i64 points {0};
        i64 x {0};
        for (auto [ab, i] : q){
            auto [a, b] = ab;
            if (broken[a]) continue; 
            points += -b;
            x++;
            on[i] = true;
            if ( ! broken[x]){
                broken[x] = true;
                for (int i : indices[x]) x -= on[i];
            }
        }

        cout << points << endl;
    }
}
