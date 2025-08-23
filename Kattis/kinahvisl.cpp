#include <iostream>
#include <string>
#include <iomanip>

using namespace std;

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);

    string x,y;
    cin >> x >> y;
    int ret {1};

    for (int i {0}; i < x.size(); ret += x[i] != y[i++]);

    cout << ret << '\n';
}
