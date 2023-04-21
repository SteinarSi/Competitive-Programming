#include <iomanip>
#include <iostream>
#include <vector>
#include <algorithm>
#include <cmath>

using namespace std;

const size_t MAGIC = 6;

double dist(pair<double,double> &a, pair<double,double> &b){ 
    double dx = abs(a.first - b.first);
    double dy = abs(a.second - b.second);
    return dx*dx + dy*dy;
}

pair<double,pair<int,int>> caesar(int from, int to, vector<pair<double,double>> &points){
    if (to - from <= 4){
        int bi = from;
        int bj = from+1;
        double best = dist(points[bi], points[bj]);
        for (int i {from}; i < to-1; i++){
            for (int j {i+1}; j < to; j++){
                double d = dist(points[i], points[j]);
                if (d < best){
                    bi = i;
                    bj = j;
                    best = d;
                }
            }
        }
        return {best, {bi, bj}};
    }

    int split = from + (to - from) / 2;
    auto ret = min(caesar(from, split, points), caesar(split, to, points));
    double x_star = (points[split-1].first + points[split].first) / 2;
    vector<int> s;
    for (int i {from}; i < to; i++) if (abs(points[i].first - x_star)*abs(points[i].first - x_star) <= ret.first) s.push_back(i);
    if (s.size() < 2) return ret;

    sort(s.begin(), s.end(), [&points](int p1, int p2){ return points[p1].second < points[p2].second; });

    for (int i {0}; i < min(MAGIC, s.size()); i++){
        for (int j {i+1}; j < min(MAGIC, s.size()); j++){
            double d = dist(points[s[i]], points[s[j]]);
            if (d < ret.first) ret = {d, {s[i], s[j]}};
        }
    }
    for (int i {MAGIC}; i < s.size(); i++){
        for (int j {i-MAGIC}; j < i; j++){
            double d = dist(points[s[i]], points[s[j]]);
            if (d < ret.first) ret = {d, {s[i], s[j]}};
        }
    }
    return ret;
}

int main(){
    cout << setprecision(10) << fixed;
    int n;
    double x, y;
    vector<pair<double,double>> points;
    while (true){
        cin >> n;
        if (n == 0) break;
        points = {};
        for (int p {0}; p < n; p++){
            cin >> x >> y;
            points.push_back({x, y});
        }
        sort(points.begin(), points.end());
        auto [p1, p2] = caesar(0, n, points).second;
        cout << points[p1].first << ' ' << points[p1].second << ' ' << points[p2].first << ' ' << points[p2].second << '\n';
    }
}