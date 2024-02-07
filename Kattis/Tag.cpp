#include <string>
#include <vector>
#include <algorithm>
#include <map>
#include <cstdint>
#include <iostream>

using namespace std;
using i64 = uint64_t;

class ID {
  public:
    map<string,i64> name2id;
    vector<string> id2name;

    void add(const string &s){
        get_id(s);
    }

    i64 get_id(const string &s){
        auto [it, placed] = name2id.emplace(s, name2id.size());
        if (placed) id2name.push_back(s);
        return it->second;
    }
};

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);

    i64 n, m, a, b;
    cin >> n >> m;
    ID id = ID();
    vector<bool> it(n, false);
    it[0] = true;
    string hunter, hunted, garbage;
    for (i64 i {0}; i < n; i++) {
        cin >> hunted;
        id.add(hunted);
    }
    vector<bool> cheater(n, false);
    vector<string> cheaters;
    for (i64 i {0}; i < m; i++) {
        cin >> hunter >> garbage >> hunted;
        a = id.get_id(hunter);
        b = id.get_id(hunted);
        if ( ! it[a] && ! cheater[a]) {
            cheater[a] = true;
            cheaters.push_back(id.id2name[a]);
        }
        it[a] = false;
        it[b] = true;
    }
    sort(cheaters.begin(), cheaters.end());
    cout << cheaters.size() << '\n';
    for (string cheat : cheaters) {
        cout << cheat << ' ';
    }
    cout << '\n';
}