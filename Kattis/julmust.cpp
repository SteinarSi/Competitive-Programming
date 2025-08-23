#include <iostream>

using namespace std;
using u64 = uint64_t;

int main(){
    ios::sync_with_stdio(false);
    u64 r;
    cin >> r;
    string s;
    u64 hi {r};
    u64 lo {0};
    u64 guess;
    u64 day {1};
    while (lo <= hi){
        guess = lo + (hi - lo) / 2;
        cout << guess*day << endl;
        cin >> s;
        if (s == "less") hi = guess-1;
        else if (s == "more") lo = guess+1;
        else return 0;
        day++;
    }
}