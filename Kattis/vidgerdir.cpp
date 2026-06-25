#include <iostream>
#include <string>
#include <vector>
#include <iomanip>
#include <algorithm>

using namespace std;

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);
    
    int n, m, x;
    char c;
    string comp;
    cin >> n >> m;
    vector<int> counts(m, 0);
    vector<pair<string,int>> pc(m);
    
    for (; n --> 0 ;) {
        for (int i {0}; i < m; i++) {
            cin >> comp >> x;
            if (i != m-1) cin >> c;
            pc[i] = {comp,x};
        }
        sort(pc.begin(), pc.end());
        for (int i {0}; i < m; i++) {
            if (pc[i].second == 0) counts[i]++;
        }
    }
    
    int worst = counts[0];
    for (int i {1}; i < m; i++) {
        worst = min(worst, counts[i]);
    }

    cout << worst << '\n';
}
