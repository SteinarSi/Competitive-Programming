#include <iostream>
#include <vector>
#include <algorithm>
#include <iomanip>

using namespace std;

int main(){
    cout << setprecision(10) << fixed;
    int n;
    double x, y;
    cin >> n;
    vector<pair<double, double>> fx(n);
    for (int i {0}; i < n; i++){
        cin >> x >> y;
        fx[i] = {x, y};
    }
    sort(fx.begin(), fx.end());
    double best, slope;
    for (int i {0}; i < n-1; i++){
        slope = abs(fx[i].second-fx[i+1].second) / abs(fx[i].first - fx[i+1].first);
        if (i == 0 || slope > best) best = slope;
    }
    cout << best << '\n';
}