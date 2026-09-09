#include <iostream>
#include <vector>

using namespace std;
using i64 = int64_t;

const i64 INF = numeric_limits<i64>::max();

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

    i64 n, m, x, y;
    cin >> n >> m;

    i64 h = m+n-1;
    vector<vector<i64>> matrix(n, vector<i64>(h));

    vector<pair<i64,i64>> bottles(n);
    for (int i {0}; i < n; i++) {
        cin >> x >> y;
        bottles[i] = {x, y};
    }
    vector<pair<i64,i64>> couriers(m);
    for (int j {0}; j < m; j++) {
        cin >> x >> y;
        couriers[j] = {x, y};
    }
    cin >> x >> y;
    for (int i {0}; i < n; i++) {
        auto [bx,by] = bottles[i];
        i64 dist2rest = abs(bx-x) + abs(by-y);
        for (int j {0}; j < m; j++) {
            auto [cx,cy] = couriers[j];
            matrix[i][j] = dist2rest + abs(cx-bx) + abs(cy-by);
        }
        for (i64 j {m}; j < h; j++) {
            matrix[i][j] = dist2rest + dist2rest;
        }
    }

    cout << hungarian(matrix, n, h) << '\n';
}
