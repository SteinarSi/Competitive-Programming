#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

void printDP(vector<vector<bool>> &dp){
    cout << "w= ";
    for (int w {0}; w < dp[0].size(); w++) cout << w << ' ';
    cout << '\n';
    for (int i {0}; i < dp.size(); i++){
        cout << i << ": ";
        for (int w {0}; w < dp[0].size(); w++){
            cout << dp[i][w] << ' ';
        }
        cout << '\n';
    }
}

vector<int> backtrack(vector<int> &data, vector<vector<pair<int, int>>> &parent){
    vector<int> ret;
    pair<int,int> curr = {parent.size()-1, parent[0].size()-1};
    while (curr.second != 0){
        pair<int,int> next = parent[curr.first][curr.second];
        if (next.second != curr.second) ret.push_back(data[curr.first-1]);
        curr = next;
    }
    return ret;
}

vector<int> sss(vector<int> &data, int target) {
    vector<vector<bool>> dp = vector<vector<bool>>(data.size()+1, vector<bool>(target+1));
    vector<vector<pair<int, int>>> parent = vector<vector<pair<int, int>>>(data.size()+1, vector<pair<int,int>>(target+1));
    for (int i {1}; i <= data.size(); i++){
        dp[i][0] = true;
        parent[i][0] = {i-1, 0};
    }   
    dp[0][0] = true;    

    for (int i {1}; i <= data.size(); i++){
        int problem = data[i-1];
        for (int w {1}; w <= target; w++){
            if (dp[i-1][w]){
                printf("%d,%d points to %d,%d\n", i, w, i-1, w);
                dp[i][w] = true;
                parent[i][w] = {i-1, w};
            }
            else if (w - problem >= 0 && dp[i-1][w - problem]){
                printf("%d,%d points to %d,%d, picking %d\n", i, w, i-1, w-problem, problem);
                dp[i][w] = true;
                parent[i][w] = {i-1, w-problem};
            }
        }
    }
    if (dp[data.size()][target]) cout << "Found answer\n";
    else cout << "No answer\n";

    return backtrack(data, parent);
}

int main(){
    cin.exceptions(ios::failbit);
    int n, d;
    cin >> n;
    int s {0};
    vector<int> data;
    for (int i {0}; i < n; i++){
        cin >> d;
        s += d;
        data.push_back(d);
    }
    vector<int> result = sss(data, s/2);
    for (int i : result){
        cout << i << '\n';
    }
}