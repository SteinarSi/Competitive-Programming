#include <iostream>
#include <vector>
#include <map>
#include <string>
#include <algorithm>

using namespace std;
using u64 = uint64_t;

string solve(string &alien_number, string &source_language, string &target_language){
    u64 source_base = source_language.size();
    u64 target_base = target_language.size();

    map<char, u64> digit_in_dec;
    for (int i {0}; i < source_base; i++) digit_in_dec.insert({source_language[i], i});

    u64 pow {1};
    u64 alien_dec {0};
    for (int i {0}; i < alien_number.size(); i++){
        alien_dec += pow * digit_in_dec[alien_number[alien_number.size()-1-i]];
        pow *= source_base;
    }

    string ret;
    for (; alien_dec; alien_dec /= target_base) ret += target_language[alien_dec % target_base];

    reverse(ret.begin(), ret.end());

    return ret;
}

int main(){
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    int n;
    cin >> n;
    string num, src, tar;
    for (int i {1}; i <= n; i++){
        cin >> num >> src >> tar;
        cout << "Case #" << i << ": " << solve(num, src, tar) << '\n';
    }
}