#include <string>
#include <vector>
#include <algorithm>
#include <map>
#include <cstdint>
#include <iostream>

using namespace std;

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);

    int n, k, c, ix;
    string cat, item;
    cin >> n >> k;

    vector<string> items(n);
    for (int i {0}; i < n; cin >> items[i++]);

    map<string,int> order;
    for (int i {0}; i < k; i++) {
        cin >> cat;
        order[cat] = i;
    }

    map<string,int> priority;
    for (int i {0}; i < k; i++) {
        cin >> cat >> c;
        ix = order[cat];
        for (; c --> 0 ;) {
            cin >> item;
            priority[item] = ix;
        }
    }

    sort(items.begin(), items.end(), [&priority](string a, string b) { return priority[a] < priority[b]; });

    for (int i {0}; i < n-1; i++) {
        cout << items[i] << ' ';
    }
    cout << items[n-1] << '\n';
}
