#include <vector>
#include <iostream>
#include <iomanip>
#include <cstdint>

using namespace std;
using u64 = uint64_t;

const u64 MAX_H = 1000001;

int main(){
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);
    
    vector<u64> hprimes;
    vector<bool> hprime(MAX_H+1,true);

    for (u64 h {5}; h <= MAX_H; h += 4) {
        if (hprime[h]) {
            hprimes.push_back(h);
            for (u64 c {1}; (4*c+1)*h <= MAX_H; c++) {
                hprime[(4*c+1)*h] = false;
            }
        }
    }

    vector<bool> semiprime(MAX_H+1,false);
    for (u64 i {0}; i < hprimes.size(); i++) {
        for (u64 j {i}; j < hprimes.size() && hprimes[i]*hprimes[j] <= MAX_H; j++) {
            semiprime[hprimes[i]*hprimes[j]] = true;
        }
    }
    vector<int> psum(MAX_H+1, 0);
    for (u64 h {5}; h <= MAX_H; h += 4) {
        psum[h] = psum[h-4] + semiprime[h];
    }
    
    u64 h;
    while ((cin >> h) && h) {
        cout << h << ' ' << psum[h] << '\n';
    }
}
