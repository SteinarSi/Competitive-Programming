#include <iostream>
#include <vector>
#include <iomanip>

using namespace std;

int main(){
    cout << setprecision(1) << fixed;
    int n;
    vector<double> xs, ys;
    double a, b;
    double degs, area;
    while (cin >> n){
        if (n == 0) break;
        xs.clear();
        ys.clear();
        degs = 0.0;
        area = 0.0;
        for (int i {0}; i < n; i++){
            cin >> a >> b;
            xs.push_back(a);
            ys.push_back(b);
        }
        xs.push_back(xs[0]);
        ys.push_back(ys[0]);
        for (int i {0}; i < n; i++){
            area += xs[i] * ys[i+1] - ys[i] * xs[i+1];
        }
        if (area < 0) cout << "CW ";
        else cout << "CCW ";
        cout << abs(area / 2) << endl;
    }  
}