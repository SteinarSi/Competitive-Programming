#include <string>
#include <vector>
#include <map>
#include <cstdint>

using namespace std;
using i64 = uint64_t;

class ID {
    map<string,i64> name2id;
    vector<string> id2name;

public:
    void add(const string &s){
        get_id(s);
    }

    i64 get_id(const string &s){
        auto [it, placed] = name2id.emplace(s, name2id.size());
        if (placed) id2name.push_back(s);
        return it->second;
    }
};

#include <iostream>
#include <iomanip>
#include <vector>

using namespace std;

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);
    cout << setprecision(10) << fixed;

    i64 c, n;
    string name, cat;

    cin >> c;
    for (;c --> 0;) {
        cin >> n;
        ID id = ID();
        vector<i64> categories(n, 1);
        for (;n --> 0;) {
            cin >> name >> cat;
            i64 i = id.get_id(cat);
            categories[i]++;
        }
        i64 prod = 1;
        for (i64 p : categories) prod *= p;

        cout << prod-1 << '\n';
    }
}
