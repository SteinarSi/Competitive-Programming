#include <iostream>
#include <iomanip>
#include <string>
#include <map>

using namespace std;

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);

    int n, v;
    string x;
    map<string,int> m;

    cin >> n;
    for (int i {0}; i < n; i++) {
        cin >> x >> v;

        auto [iterator, inserted] = m.try_emplace(x, v);
        if (!inserted) { iterator->second += v; }
    }

    for (auto [name,total] : m) {
        if (total > v) {
            x = name;
            v = total;
        }
    }
    cout << x << '\n';
}
