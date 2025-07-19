#include <iostream>
#include <iomanip>
#include <vector>
#include <complex>
#include <cstdint>

using namespace std;
using i64 = int64_t;

const double PI = 3.141592653589793238;

void fft(vector<complex<double>> &xs, bool invert) {
    int n = xs.size();
    if (n == 1) return;

    vector<complex<double>> xs0(n / 2), xs1(n / 2);
    for (int i = 0; 2 * i < n; i++) {
        xs0[i] = xs[2*i];
        xs1[i] = xs[2*i+1];
    }
    fft(xs0, invert);
    fft(xs1, invert);

    double ang = 2 * PI / n * (invert ? -1 : 1);
    complex<double> w(1), wn(cos(ang), sin(ang));

    for (int i = 0; 2 * i < n; i++) {
        xs[i] = xs0[i] + w * xs1[i];
        xs[i + n/2] = xs0[i] - w * xs1[i];
        if (invert) {
            xs[i] /= 2;
            xs[i + n/2] /= 2;
        }
        w *= wn;
    }
}

vector<i64> poly_multiply(vector<i64> const& xs, vector<i64> const& ys) {
    vector<complex<double>> fx(xs.begin(), xs.end()), fy(ys.begin(), ys.end());
    int n = 1;
    while (n < xs.size() + ys.size()) n <<= 1;

    fx.resize(n);
    fy.resize(n);

    fft(fx, false);
    fft(fy, false);
    for (int i = 0; i < n; i++) {
        fx[i] *= fy[i];
    }
    fft(fx, true);

    vector<i64> result(n);
    for (int i = 0; i < n; i++) {
        result[i] = round(fx[i].real());
    }

    return result;
}

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);
    cout << setprecision(10) << fixed;
    
    i64 t, n, m;
    cin >> t;
    while (t-->0) {
        cin >> n;
        vector<i64> xs(n+1);
        for (int i {0}; i <= n; cin >> xs[i++]);
        cin >> m;
        vector<i64> ys(m+1);
        for (int i {0}; i <= m; cin >> ys[i++]);

        vector<i64> zs = poly_multiply(xs,ys);

        int end = n+m+1;
        while (end > 1 && zs[end-1] == 0) end--;
        cout << end-1 << '\n';
        for (int i {0}; i < end; i++) {
            cout << zs[i] << ' ';
        }
        cout << '\n';
    }
    
}
