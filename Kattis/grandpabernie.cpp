#include <iostream>
#include <iomanip>
#include <map>
#include <string>
#include <algorithm>
#include <vector>

using namespace std;

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);
    cout << setprecision(10) << fixed;

    int n, q, year, time;
    string country;

    map<string,int> id;
    vector<vector<int>> memory;

    cin >> n;
    for (int i {0}; i < n; i++) {
        cin >> country >> year;
        auto [it, placed] = id.emplace(country, id.size());
        if (placed) memory.push_back({year});
        else memory[it->second].push_back(year);
    }

    for (int i {0}; i < memory.size(); i++) {
        sort(memory[i].begin(), memory[i].end());
    }

    cin >> q;

    for (int i {0}; i < q; i++) {
        cin >> country >> time;
        cout << memory[id.at(country)][time-1] << '\n';
    }

}