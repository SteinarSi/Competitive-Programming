#include <iostream>
#include <vector>
#include <iomanip>
#include <set>

using namespace std;
using i64 = int64_t;

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);
    cout << setprecision(10) << fixed;

    int n, m, c;
    vector<int> samples;
    cin >> n >> m >> c;
    int j;
    for (int i {0}; i < n; i++){
        cin >> j;
        samples.push_back(j);
    }

    vector<int> result;
    multiset<int> sub_samples;

    for (int i {0}; i < m; i++){
        sub_samples.insert(samples[i]);
    }
    if ((*sub_samples.rbegin() - *sub_samples.begin()) <= c){
        result.push_back(1);
    }

    for (int i {1}; i <= n - m; i++){
        sub_samples.insert(samples[i+m-1]);
        sub_samples.erase(sub_samples.find(samples[i-1]));

        if ((*sub_samples.rbegin() - *sub_samples.begin()) <= c){
            result.push_back(i+1);
        }
    }
    for (int j : result){
        cout << j << '\n';
    }

    if (! result.size()){
        cout << "NONE\n";
    }
}
