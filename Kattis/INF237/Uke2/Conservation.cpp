#include <iostream>
#include <string>
#include <vector>
#include <iomanip>
#include <set>
#include <array>
#include <algorithm>

using namespace std;
using i64 = int64_t;

tuple<vector<vector<int>>, vector<int>, vector<bool>> build_graph(){
    int N, K;
    cin >> N >> K;

    int from;
    int to;
    vector<vector<int>> outgoing(N);
    vector<int> incoming(N, 0);
    vector<bool> lab;
    int l;
    for (int i {0}; i < N; i++){
        cin >> l;
        if (l == 1){
            lab.push_back(true);
        }else{
            lab.push_back(false);
        }
    }

    for (int i {0}; i < K; i++){
        cin >> from >> to;
        from--;
        to--;
        outgoing[from].push_back(to);
        incoming[to]++;
    }
    return {outgoing, incoming, lab};
}

int solve(vector<vector<int>> &outgoing, vector<int> incoming, vector<bool> lab, bool contains){
    vector<int> S1;
    vector<int> S2;
    vector<int> result;
    for(int v {0}; v < incoming.size(); v++){
        if (incoming[v] == 0){
            if (lab[v]) S1.push_back(v);
            else S2.push_back(v);
        }
    }

    int current_lab {0};
    int swaps {0};
    int u;
    while ( (! S1.empty()) || (! S2.empty())){
        if (contains){
            if(S2.empty()){
                swaps++;
                contains = ! contains;
                continue;
            }
            u = S2.back();
            S2.pop_back();
        }
        else{
            if(S1.empty()){
                swaps++;
                contains = ! contains;
                continue;
            }
            u = S1.back();
            S1.pop_back();
        }

        result.push_back(u);
        for (int v : outgoing[u]){
            if (--incoming[v] == 0){
                if (lab[v]) S1.push_back(v);
                else S2.push_back(v);
            }
        }
    }
    return swaps;
}

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);
    cout << setprecision(10) << fixed;

    int N;
    cin >> N;
    for(int i {0}; i < N; i++){
        auto [outgoing, incoming, lab] = build_graph();
        cout << min(solve(outgoing, incoming, lab, true), solve(outgoing, incoming, lab, false)) << '\n';
    }
}