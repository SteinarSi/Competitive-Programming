#include <iostream>
#include <vector>
#include <cmath>

using namespace std;

int main(){
    cin.exceptions(ios::failbit);
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
    for (int bit {0}; bit < pow(2, n); bit++){
        independent = true;
        for (int j {0}; j < n; j++){
            if ((bit & (1 << j)) == 0){
                continue;
            } 
            for (int k : current){
                if (graph[j][k]) {
                    independent = false;
                    break;
                }
            }
            if ( ! independent) break;
            current.push_back(j);
        }

        if (independent) s += 1;
        current.clear();
    }
    
    cout << s << endl;
}