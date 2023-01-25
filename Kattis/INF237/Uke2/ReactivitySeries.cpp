#include <iostream>
#include <string>
#include <vector>
#include <iomanip>
#include <set>
#include <array>

using namespace std;
using i64 = int64_t;

array<vector<set<int>>, 2> build_graph(){
    int N, K;
    cin >> N >> K;

    int from;
    int to;
    vector<set<int>> outgoing(N);
    vector<set<int>> incoming(N);

    for (int i {0}; i < K; i++){
        cin >> from >> to;
        outgoing[from].insert(to);
        incoming[to].insert(from);
    }
    return {outgoing, incoming};
}

/*
L ← Empty list that will contain the sorted elements
S ← Set of all nodes with no incoming edge

while S is not empty do
    remove a node n from S
    add n to L
    for each node m with an edge e from n to m do
        remove edge e from the graph
        if m has no other incoming edges then
            insert m into S

if graph has edges then
    return error   (graph has at least one cycle)
else 
    return L   (a topologically sorted order)
*/

void solve(vector<set<int>> &outgoing, vector<set<int>> &incoming){
    set<int> S;
    vector<int> result;
    for(int v {0}; v < incoming.size(); v++){
        if (incoming[v].empty()){
            S.insert(v);
        }
    }   

    int u;
    while( ! S.empty()){
        if (S.size() > 1){
            printf("back to the lab");
            return;
        }
        u = *S.begin();
        S.erase(u);
        result.push_back(u);
        for (int v : outgoing[u]){
            incoming[v].erase(u);
            if (incoming[v].size() == 0){
                S.insert(v);
            }
        }
    }
    for (int n : result){
        cout << n << ' ';
    }
    cout << '\n';
}

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);
    cout << setprecision(10) << fixed;

    auto [outgoing, incoming] = build_graph();
    solve(outgoing, incoming);
}

/*
7 10
2 3
3 4
4 6
5 6
3 6
3 5
0 1
0 2
0 3
1 2

back to the lab


4 6
0 3
0 2
0 1
1 2
1 3
2 3

0 1 2 3


5 7
0 3
0 2
0 1
1 2
1 3
2 3
4 3

back to the lab


5 4
3 0
3 2
0 2
4 1

back to the lab



5 5
3 0
3 2
0 2
4 1
0 4

back to the lab


5 5
3 0
3 2
0 2
4 1
2 4

3 0 2 4 1

*/

