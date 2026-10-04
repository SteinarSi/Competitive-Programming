#include <iostream>
#include <iomanip>
#include <vector>

using namespace std;

bool attempt(int n, vector<int> &xs, vector<bool> &backwheel, int d) {
    int length = xs[d] - xs[0];

    bool valid = true;
    int i {0};
    int j {d};

    while (true) {
        if (j >= n) {
            for (int k {i}; k < n; k++) {
                if (!backwheel[k]) {
                    valid = false;
                    break;
                }
            }
            break;
        }
        if (i == j) j++;
        else if (backwheel[i]) i++;
        else if (xs[j]-xs[i] == length) {
            backwheel[j] = true;
            i++;
            j++;
        }
        else if (xs[j]-xs[i] > length) {
            valid = false;
            break;
        }
        else j++;
    }

    backwheel.assign(n, false);

    return valid;
}

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);

    int n;
    cin >> n;
    vector<int> xs(n);
    for (int i {0}; i < n; cin >> xs[i++]);

    vector<bool> backwheel(n, false);

    vector<int> ret;

    for (int d {1}; d <= n/2; d++) {
        if (xs[d] == xs[d-1]) continue;
        if (attempt(n, xs, backwheel, d)) ret.push_back(d);
    }

    cout << ret.size() << '\n';
    for (int d : ret) {
        cout << xs[d]-xs[0] << '\n';
    }
}