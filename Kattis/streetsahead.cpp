#include <iostream>
#include <string>
#include <vector>
#include <iomanip>
#include <map>

using namespace std;
using i64 = int64_t;

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);
    cout << setprecision(10) << fixed;


    map<string, long long> name_to_id;
    vector<string> id_to_name;
    auto get_id = [&](const string &s) {
        auto [it, placed] = name_to_id.emplace(s, name_to_id.size());
        if (placed) id_to_name.push_back(s);
        return it->second;
    };
    int n, q;
    string a, b;
    cin >> n >> q;
    for (int i {0}; i < n; i++){
        cin >> a;
        get_id(a);
    }
    for (int j {0}; j < q; j++){
        cin >> a >> b;
        cout << abs(get_id(a) - get_id(b)) - 1 << '\n';
    }
}
