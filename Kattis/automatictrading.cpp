#include <iostream>
#include <iomanip>
#include <string>

using namespace std;

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);
    cout << setprecision(10) << fixed;

    string xs;
    int n, a, b, r;
    cin >> xs >> n;

    // The fact that this solution is fast enough is just C++ bullshit, 
    // using C++ to get away with bad algorithms should be considered cheating
    for (int i {n}; i --> 0;) {
        cin >> a >> b;
        r = 0;
        while (xs[a++] == xs[b++]) r++;
        cout << r << '\n';
    }
}
