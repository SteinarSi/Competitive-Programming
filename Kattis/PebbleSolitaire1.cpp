#include <vector>
#include <iostream>
#include <iomanip>
#include <string>
#include <bit>

using namespace std;

vector<int> pow2 {1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,16384,32768,65536,131072,262144,524288,1048576,2097152,4194304,8388608};

const int SIZE = 12;

int solve (vector<int> &opt, int u) {        
    if (opt[u]) return opt[u];
    int popcount {0};
    int best {SIZE};

    for (int i {0}; i < SIZE; i++) {
        if ((1 << i) & u) {
            popcount += 1;
            if (i+2 < SIZE && (1 << (i+1)) & u && !((1 << (i+2)) & u)) {
                int v = u ^ (7 << (i));
                best = min(best, solve(opt, v));
            }
            if (i-2 >= 0 && (1 << (i-1)) & u && !((1 << (i-2)) & u)) {
                int v = u ^ (7 << (i-2));
                best = min(best, solve(opt, v));
            }
        }
    }
    best = min(best, popcount);
    opt[u] = best;
    return best;
}

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);

    int n, u;
    string s;
    cin >> n;
    for (int c {0}; c < n; c++) {
        vector<int> opt(8388608, 0);
        cin >> s;
        u = 0;
        for (int i {SIZE-1}; i >= 0; i--) {
            u += (s[i] == 'o') * pow2[i];
        }
        cout << solve(opt, u) << '\n';
    }
}
