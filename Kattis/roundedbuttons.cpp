#include <iostream>
#include <vector>

using namespace std;


bool inside(double x, double y, double w, double h, double r, double a, double b){
    vector<pair<double, double>> circles {{x+r, y+r}, {x+r, y+h-r}, {x+w-r, y+r}, {x+w-r, y+h-r}};
    for (auto [sx, sy] : circles){
        if (abs(sx-a)*abs(sx-a) + abs(sy-b)*abs(sy-b) <= r * r) return true; // TODO
    }
    if (a >= x+r && a <= x+w-r && b >= y && b <= y+h) return true;
    if (b >= y+r && b <= y+h-r && a >= x && a <= x+w) return true;
    return false;
}


int main(){
    int n, m;
    double x, y, w, h, r, a, b;
    cin >> n;
    for (int i {0}; i < n ; i++){
        cin >> x >> y >> w >> h >> r >> m;
        for (int j {0}; j < m; j++){
            cin >> a >> b;
            if (inside(x, y, w, h, r, a, b)) cout << "inside\n";
            else cout << "outside\n";
        }
        cout << '\n';
    }
}