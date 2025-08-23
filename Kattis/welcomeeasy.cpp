#include <iostream>
#include <string>
#include <vector>
#include <iomanip>
#include <cstdint>

using namespace std;
using u64 = uint64_t;

string TARGET = "welcome to code jam";
u64 N = TARGET.size();
u64 MAX_LENGTH = 501;
u64 MOD = 10000;

/*
OPT[i][j] = "antall måter å få de første i bokstavene i sekvensen med de første j bokstavene i inputtet"

OPT[_][0] = 0
OPT[0][_] = 1

OPT[i][j] = OPT[i][j-1] (+ OPT[i-1][j-1] if s[i] == t[i])

*/

u64 solve(vector<vector<u64>> &opt, string &source) {
    u64 M = source.size();
    for (u64 i {1}; i <= N; i++) {
        for (u64 j {1}; j <= M; j++) {
            opt[i][j] = opt[i][j-1];
            if (source[j-1] == TARGET[i-1]) opt[i][j] += opt[i-1][j-1];
            if (opt[i][j] > MOD) opt[i][j] -= MOD;
        }
    }

    return opt[N][M];
}

void format(int c, u64 solulu) {
    cout << "case #" << c << ": ";
    string a = to_string(solulu % 10000);
    for (u64 i {4}; i > a.size(); i--) {
        cout << '0';
    }
    cout << a << '\n';
}

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);
    
    vector<vector<u64>> opt(TARGET.size()+1, vector<u64>(MAX_LENGTH+1, 0));
    for (u64 i {0}; i <= MAX_LENGTH; i++) {
        opt[0][i] = 1;
    }

    string source;
    int t;
    cin >> t;
    getline(cin, source);

    for (int c {1}; c <= t; c++){
        getline(cin, source);
        format(c, solve(opt, source));
    }
}
