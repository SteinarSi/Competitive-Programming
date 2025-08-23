#include <iostream>
#include <vector>
#include <iomanip>

using namespace std;
using i64 = int64_t;

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);
    cout << setprecision(10) << fixed;

    int c;
    int d;
    int n;
    long long s;
    long long summ;
    int result;
    vector<int> mods;
    cin >> c;
    for (int p {0}; p < c; p++){
        cin >> d >> n;
        mods = vector(d, 0);
        summ = 0;
        result = 0;
        for (int i {0}; i < n; i++){
            cin >> s;
            summ += s;
            mods[summ % d] += 1;
        }

        result += mods[0]*(mods[0]+1) / 2;     // _|_|_|, like mange sekvenser som skilletegn
        for (int i {1}; i < mods.size(); i++){
            result += mods[i]*(mods[i]-1)/2;   // |_|_| vil regne ut antall sekvenser basert på antall skilletegn
        }
        cout << result << '\n';
    }
}
