#include <iostream>
#include <vector>
#include <algorithm>
#include <cmath>

using namespace std;

double angle(pair<double,double> u, pair<double,double> v){
    return atan2(v.second-u.second, v.first-u.first);
}

int main(){
    int c, n, botleft;
    cin >> c;
    double x, y, xm, ym;
    pair<double,double> mid;
    vector<pair<double, double>> points;
    vector<int> indices;
    for (int ccc {0}; ccc < c; ccc++){
        points = {};
        indices = {};
        cin >> n;
        xm = 0;
        ym = 0;
        for (int i {0}; i < n; i++){
            cin >> x >> y;
            points.push_back({x, y});
            indices.push_back(i);
            xm += x;
            ym += y;
        }

        mid = {xm/n, ym/n};
        sort(indices.begin(), indices.end(), [botleft, &mid, &points](int i, int j){
            return angle(mid, points[i]) < angle(mid, points[j]) 
               || (angle(mid, points[i]) == angle(mid, points[j]) && points[i].first < points[j].first);
        });

        for (int i {0}; i < n; i++){
            cout << indices[i] << ' ';
        }
        cout << '\n';
    }
}


/*
2
9 0 0 0 1 0 2 1 0 1 1 1 2 2 0 2 1 2 2
5 0 1 1 0 2 1 3 0 4 1



*/