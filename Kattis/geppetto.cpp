#include <iostream>
#include <vector>
#include <cmath>
#include <iomanip>

using namespace std;

int main(){
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);
    cout << setprecision(10) << fixed;

    int n, m, a, b;
    cin >> n >> m;
    vector<vector<bool>> graph(n, vector<bool>(n, false));
    for (int i {0}; i < m; i++){
        cin >> a >> b;
        a--; b--;
        graph[a][b] = true;
        graph[b][a] = true;
    }
    vector<int> current;
    bool independent;
    int s = 0;
    for (int bit {0}; bit < (1 << n); bit++){
        independent = true;
        for (int j {0}; j < n; j++){
            if ((bit & (1 << j)) == 0) continue;
            for (int k : current){
                if (graph[j][k]) {
                    independent = false;
                    break;
                }
            }
            if ( ! independent) break;
            current.push_back(j);
        }
        s += independent;
        current.clear();
    }
    
    cout << s << endl;
}
