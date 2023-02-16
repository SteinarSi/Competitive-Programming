#include <iostream>
#include <vector>
#include <iomanip>

using namespace std;

void find_reachables(int u, vector<bool> &reachable, vector<vector<int>> &graph){
    reachable[u] = true;
    for (int v : graph[u]){
        if (reachable[v]) continue;
        find_reachables(v, reachable, graph);
    }
}

void dfs(int u, vector<bool> &visited, vector<bool> &stack, vector<bool> &reachable, vector<vector<int>> &graph, vector<int> &postorder){
    visited[u] = true;
    stack[u] = true;
    for (int v : graph[u]){
        if (stack[v]){
            cout << "inf\n";
            exit(0);
        }
        if (visited[v] || !reachable[v]) continue;
        dfs(v, visited, stack, reachable, graph, postorder);
    }
    postorder.push_back(u);
    stack[u] = false;
}

template <class T>
void print_list(vector<T> list){
    for(auto a : list){
        cout << a << ' ';
    }
    cout << '\n';
}

int main(){
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);
    cout << setprecision(10) << fixed;

    int n, m;
    cin >> n >> m;

    vector<vector<int>> graph(10000);
    vector<vector<int>> backwards(10000);

    int fro, to;
    for(int i {0}; i < m; i++){
        cin >> fro >> to;
        graph[fro-1].push_back(to-1);
        backwards[to-1].push_back(fro-1);
    }

    // Finn noder som realistisk kan komme seg til 2.
    vector<bool> reachable(10000, false);
    find_reachables(1, reachable, backwards);

    vector<int> postorder;
    vector<bool> visited (10000, false);
    vector<bool> stack   (10000, false);

    // Finn sykler som kan nå 2, og lag en postorder
    dfs(0, visited, stack, reachable, graph, postorder);


    vector<long long> opt (10000, -9999999999999999);
    opt[1] = 1;
    long long s;
    bool too_long = false;
    for(int u : postorder){
        if (u == 1)  continue;
        s = 0;
        for(int v : graph[u]){
            if (reachable[v]){
                 s += opt[v];
            }
        } 
        if (s > 1000000000) too_long = true;
        opt[u] = s % 1000000000;
    }

    string result = to_string(opt[0]);
    if (too_long){
        for (int i {0}; i < 9 - result.size(); i++){
            cout << '0';
        }
    }
    cout << result << '\n';
}

/*
3 5
1 2
1 2
1 3
3 2
3 2

4



3 3
1 3
3 3
1 2

1


2 1
1 2

1



4 4
1 3
3 2
1 4
4 2

2



3 3
1 3
3 2
3 3

inf



2 2
1 2
2 2

inf


2 2
1 2
1 1

inf


4 4
1 3
3 4
4 3
1 2

1



4 5
1 4
4 2
4 2
1 3
3 2

3


4 3
1 4
4 2
2 3

1


4 4
1 4
4 2
2 3
3 2

inf


5 7
1 4 
4 2
1 3
3 4
3 2
3 5
5 5

3


8 11
1 4
4 5
5 2
1 6
4 6
6 5
5 7
1 8
8 7
7 2
2 100         Er det et krav at alle nodene må ha indeks mindre eller lik n? Nei. Men er alle testene skrevet sånn? Vet ikke.

7


2 1
2 1

0
*/
