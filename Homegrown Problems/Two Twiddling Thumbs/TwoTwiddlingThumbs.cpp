#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;


struct WeightCombination{
    vector<int> used_indices;
};

struct WeightOptions{
    int weight;
    vector<WeightCombination> options;

    vector<WeightCombination> merge(){
        vector<WeightCombination> ret;
        while (options.size() >= 3){
            if (options.end()[-1].used_indices.size() < options.end()[-2].used_indices.size()){
                swap(options.end()[-1], options.end()[-2]);
            }
            WeightCombination next = move(options.back());
            options.pop_back();
            next.used_indices.insert(next.used_indices.end(), options.back().used_indices.begin(), options.back().used_indices.end());
            options.pop_back();
            ret.push_back(move(next));
        }
        return ret;
    }
};

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
                dp[i][w] = true;
                parent[i][w] = {i-1, w};
            }
            else if (w - problem >= 0 && dp[i-1][w - problem]){
                dp[i][w] = true;
                parent[i][w] = {i-1, w-problem};
            }
        }
    }
    if (dp[data.size()][target]) return backtrack(data, parent);
    else return vector<int>();
}

int main(){
    int n, d;
    int s = 0;
    cin >> n;
    vector<int> diff_count(n, 0);
    vector<WeightOptions> combs(7201);
    vector<int> data;
    for (int w {0}; w < 7201; w++){
        combs[w].weight = w;
    }

    for (int i {0}; i < n; i++){
        cin >> d;
        ++diff_count[d];
        data.push_back(d);
        s += d;
        combs[d].options.push_back(WeightCombination{vector<int>(i)});
    }
    if (s % 2 == 1) {
        cout << "commence the thumb-twiddling\n";
        return 0;
    }

    vector<int> result = sss(data, s / 2);
    if (result.size() == 0) cout << "commence the thumb-twiddling\n";
    else {
        for (int i : result) cout << i << '\n';
    }
/*
    for (d = 1; d < 3601; d++){
        vector<WeightCombination> merged = combs[d].merge();
        combs[2*d].options.insert(combs[2*d].options.end(), make_move_iterator(merged.begin()), make_move_iterator(merged.end()));
        for (int i {0}; i < combs[d].options.size(); i++){
            data.push_back(combs[d].weight);
        }
    }
    */

    //for (int picked : sss(data, s / 2)){
    //    cout << picked << '\n'; //TODO
        /*
        for (int i = 0; i < combs[picked].options[combs[picked].options.size()-1].used_indices.size(); i++){
            int index = combs[picked].options[combs[picked].options.size()-1].used_indices[i];
            cout << index+1 << '\n';
        }
        combs[picked].options.pop_back();
        */
    //}
}