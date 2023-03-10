#include <bits/stdc++.h>

#include <iostream>
#include <string>
#include <vector>
#include <iomanip>

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

}

