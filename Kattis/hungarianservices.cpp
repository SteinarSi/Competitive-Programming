#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;
using i64 = int64_t;

const i64 INF = numeric_limits<i64>::max();
const i64 M = 1000000000;

bool ckmin(i64& a, i64 b) {
    return b < a ? a = b, true : false;
}

i64 hungarian(const vector<vector<i64>> &graph, i64 n, i64 m) {
    vector<i64> job(m + 1, -1);
    vector<i64> ys(n); 
    vector<i64> potentials(m + 1);

    for (i64 nCur = 0; nCur < n; ++nCur) {
        i64 mCur = m;
        job[mCur] = nCur;
        vector<i64> minTo(m + 1, INF);
        vector<i64> prev(m + 1, -1);
        vector<bool> seen(m + 1);
        while (job[mCur] != -1) {
            seen[mCur] = true;
            const i64 j = job[mCur];
            i64 delta = INF;
            i64 mNext;
            for (i64 w = 0; w < m; ++w) {
                if (!seen[w]) {
                    if (ckmin(minTo[w], graph[j][w] - ys[j] - potentials[w])) {
                        prev[w] = mCur;
                    }
                    if (ckmin(delta, minTo[w])) {
                        mNext = w;
                    }
                }
            }
            for (i64 w = 0; w <= m; ++w) {
                if (seen[w]) {
                    ys[job[w]] += delta;
                    potentials[w] -= delta;
                } else {
                    minTo[w] -= delta;
                }
            }
            mCur = mNext;
        }
        for (i64 w; mCur != m; mCur = w) {
            job[mCur] = job[w = prev[mCur]];
        }
    }
    return - potentials[m];
}

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);

    i64 n, m, r, a, b, c;
    cin >> n >> m >> r;

    vector<vector<i64>> matrix(n, vector<i64>(m,0));

    for (; r --> 0 ;) {
        cin >> a >> b >> c;
        matrix[a-1][b-1] = - M - c;
    }

    i64 ans = - hungarian(matrix, n, m);
    i64 unmatched = n - ans / M;
    i64 suitability = ans % M;

    cout << unmatched << ' ' << suitability << '\n';
}
