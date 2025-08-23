#include <vector>
#include <iostream>
#include <iomanip>

using namespace std;

vector<vector<int>> preprocess(int n, vector<int> &xs) {
    vector<int> right(n+1);
    vector<int> stack;

    for (int i {n}; i >= 0; i--) {
        while (stack.size() && xs[stack[stack.size()-1]] < xs[i]) stack.pop_back();

        if (stack.size()) right[i] = stack[stack.size()-1];
        else right[i] = -1;
    
        stack.push_back(i);
    }

    int m = 1;
    while ((1 << m) <= (n+1)) m++;
    vector<vector<int>> jumps(n+1, vector<int>(m, -1));

    for (int i {1}; i <= n; i++) {
        jumps[i][0] = right[i];
    }
    for (int j {1}; j < m; j++) {
        for (int i {1}; i <= n; i++) {
            if (jumps[i][j-1] != -1) {
                jumps[i][j] = jumps[jumps[i][j-1]][j-1];
            }   
        }
    }

    return jumps;
}

int jump(vector<vector<int>> &jumps, int s, int d) {
    int power = 0;
    while (d > 0) {
        if (d & 1) {
            s = jumps[s][power];
            if (s == -1) break;
        }
        d >>= 1;
        power++;
    }
    return s;
}

void play(vector<vector<int>> &jumps, int &q) {
    int s, d;
    while (q --> 0) {
        cin >> s >> d;
        int r = jump(jumps, s, d);
        if (r == -1) cout << "leik lokid\n";
        else cout << r << '\n';
    }
}

int main(){
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);

    int n, q, s, d, r;
    cin >> n >> q;
    vector<int> xs(n+1);
    for (int i {1}; i <= n; cin >> xs[i++]);

    vector<vector<int>> jumps = preprocess(n, xs);

    while (q --> 0) {
        cin >> s >> d;
        r = jump(jumps, s, d);
        if (r == -1) cout << "leik lokid\n";
        else cout << r << '\n';
    }
}
