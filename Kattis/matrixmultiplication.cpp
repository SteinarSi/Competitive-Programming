#pragma GCC optimize("O3,unroll-loops")
#pragma GCC target("avx2,bmi,bmi2,popcnt")

#include <iostream>
#include <vector>

using namespace std;

vector<int> A(1000000);
vector<int> B(1000000);
vector<int> C(1000000);
int tc;
int n;
int x;
int y;
int m;

void parse() {
    cin >> n >> x >> y >> m;
    for (int i {0}; i < n; i++) {
        for (int j {0}; j < x; j++) {
            cin >> A[i*x + j];
        }
    }
    for (int i {0}; i < y; i++) {
        for (int j {0}; j < m; j++) {
            cin >> B[j*y + i];
        }
    }
    for (int i {0}; i < n; i++) {
        for (int j {0}; j < m; j++) {
            cin >> C[i*m + j];
        }
    }
}

void verify() {
    if (x != y) {
        cout << "Inner matrix dimensions must agree\n";
        return;
    }

    for (int i {0}; i < n; i++) {
        for (int j {0}; j < m; j++) {
            int curr = 0;
            for (int k {0}; k < x; k++) curr += A[i*x + k] * B[j*y + k];
            if (curr != C[i*m + j]) {
                cout << "WA\n";
                return;
            }
        }
    }
    cout << "AC\n";
}

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);

    cin >> tc;
    for (; tc --> 0 ;) {
        parse();
        verify();
    }
}
