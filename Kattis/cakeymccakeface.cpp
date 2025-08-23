#include <iostream>
#include <vector>
#include <iomanip>
#include <map>

using namespace std;
using i64 = int64_t;
using u64 = uint64_t;

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);
    cout << setprecision(10) << fixed;

    int n, m;
    cin >> n >> m;
    vector<u64> ns(n);
    vector<u64> ms(m);
    for (int i {0}; i < n; cin >> ns[i++]);
    for (int j {0}; j < m; cin >> ms[j++]);

    map<u64, u64> counts;
    int bound {0};
    for (int i {0}; i < n; i++){
        while (bound < n && ms[bound] < ns[i]) bound++;
        for (int j {bound}; j < m; j++){
            if (ns[i] <= ms[j]) counts[ms[j]-ns[i]]++;
        }
    }
    u64 secret_time = 0;
    u64 secret_occs = 0;
    for (auto [time, occs] : counts){
        if (occs > secret_occs){
            secret_time = time;
            secret_occs = occs;
        }
    }
    cout << secret_time << endl;
}
