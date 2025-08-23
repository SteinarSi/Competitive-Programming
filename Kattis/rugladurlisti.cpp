#include <vector>
#include <set>
#include <iostream>

using namespace std;

void divide_and_conquer(vector<int> &answer, vector<pair<int,vector<int>>> &ranges) {
    vector<int> query;
    for (int i {0}; i < ranges.size(); i++) {
        int n = ranges[i].second.size();
        if (n <= 1) {
            for (int j {0}; j < n; j++) {
                answer[ranges[i].first + j] = ranges[i].second[j];
            }
        }
        else {
            for (int j {ranges[i].first}; j < ranges[i].first + n / 2; j++) {
                query.push_back(j);
            }
        }
    }

    if (query.size()) {
        cout << "? " << query.size() << ' ';
        for (int q : query) cout << q << ' ';
        cout << '\n';

        set<int> shuffled;
        int x;
        for (int i {0}; i < query.size(); i++) {
            cin >> x;
            shuffled.insert(x);
        }

        vector<pair<int,vector<int>>> next;
        for (int i {0}; i < ranges.size(); i++) {
            if (ranges[i].second.size() <= 1) continue;
            vector<int> left;
            vector<int> right;
            for (int x : ranges[i].second) {
                if (shuffled.count(x)) left.push_back(x); 
                else right.push_back(x);
            }
            next.push_back({ranges[i].first, left});
            next.push_back({ranges[i].first+left.size(), right});
        }

        divide_and_conquer(answer, next);
    }
}

int main() {
    int n;
    cin >> n;

    vector<int> answer(n+1);

    vector<pair<int,vector<int>>> start = {{1, vector<int>(n)}};
    for (int i {1}; i <= n; start[0].second[i-1] = i, i++);

    divide_and_conquer(answer, start);

    cout << "! ";
    for (int i {1}; i <= n; i++) cout << answer[i] << ' ';
    cout << '\n';
}
