#include <vector>
#include <iostream>
#include <iomanip>

using namespace std;

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);

    int n;
    cin >> n;
    vector<int> opus(n);
    vector<int> order(n);
    vector<int> temp(n-1);
    for (int i {0}; i < n; i++) {
        cin >> opus[i];
        order[i] = i+1;
    }

    do {
        int s = (opus[order[0]-1]-1) % n;
        if (s < n-1) {
            copy(order.begin() + s + 1, order.end(), temp.begin());
        }
        if (s > 0) {
            copy(order.begin(), order.begin() + s, temp.begin() + (n-s-1));
        }
        copy(temp.begin(), temp.begin() + (n-1), order.begin());
    }
    while (n --> 2);

    cout << order[0] << '\n';
}
