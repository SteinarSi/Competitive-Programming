#include <vector>
#include <iostream>
#include <iomanip>

using namespace std;

int main(){
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    int n, k, i;
    cin >> n >> k;

    vector<bool> prime(n+1, true);
    
    int p {2};
    while (k){
        while ( ! prime[p]) p++;
        for (i = p; i <= n && k; i += p) {
            if (prime[i]){
                prime[i] = false;
                k--;
            }
        }
    }

    cout << i - p << '\n';
}
