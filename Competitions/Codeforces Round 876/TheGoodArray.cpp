#include <iostream>
#include <string>
#include <vector>
#include <iomanip>

using namespace std;

int ceil(int a, int b){
    return (a + b - 1) / b;
}

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);

    int t;
    cin >> t;
    for (int c {0}; c < t; c++){
        int n, k;
        cin >> n >> k;
        vector<bool> set(n, false);
        int ones = 0;
        int inv  = 0;
        for (int i {0}; i < n; i++){
            if (ones < ceil(i+1, k)){
                set[i] = true;
                ones++;
            }
        }
        for (int i {n-1}; i >= 0; i--){
            if (set[i]) inv++;
            if (inv < ceil(n-i, k)){
                ones++;
                inv++;
            }
        }
        cout << ones << endl;
    }
}
