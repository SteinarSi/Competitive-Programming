#include <vector>
#include <iostream>
#include <algorithm>

using namespace std;

int main(){
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    int kids, toys, duration, events;
    cin >> kids >> toys >> duration >> events;

    if (kids > toys) {
        cout << "impossible\n";
        return 0;
    }

    vector<int> current_toy(kids, -1);
    vector<int> played_since(kids, -1);
    vector<vector<int>> play_time(kids, vector<int>(toys, 0));
    vector<vector<int>> kid_prefs(kids);

    vector<int> unmatched_kids;

    int time, kid, toy;
    for (int i {0}; i < events; ++i){
        cin >> time >> kid >> toy;
        kid--; toy--;

        if (current_toy[kid] != -1){
            play_time[kid][current_toy[kid]] += time - played_since[kid];
        }

        if (play_time[kid][toy] <= 0 && toy != -1){
            kid_prefs[kid].push_back(toy);
        }

        if (toy == -1){
            current_toy[kid] = -1;
        }
        else{
            current_toy[kid] = toy;
            played_since[kid] = time;
        }
    }

    for (int kid {0}; kid < kids; ++kid){
        unmatched_kids.push_back(kid);
        if (current_toy[kid] != -1) play_time[kid][current_toy[kid]] += duration - played_since[kid];
        reverse(kid_prefs[kid].begin(), kid_prefs[kid].end());
    }

    vector<int> next_toy_index(kids, 0);

    vector<int> kid_to_toy(kids, -1);
    vector<int> toy_to_kid(toys, -1);

    while (unmatched_kids.size() > 0){
        kid = unmatched_kids[unmatched_kids.size()-1];
        unmatched_kids.pop_back();

        if (kid_prefs[kid].size() > 0){
            toy = kid_prefs[kid][kid_prefs[kid].size()-1];
            kid_prefs[kid].pop_back();
        }
        else{
            for (toy = next_toy_index[kid]; play_time[kid][toy] > 0; ++toy);
            next_toy_index[kid] = toy+1;
        } 

        if (toy_to_kid[toy] == -1) {
            toy_to_kid[toy] = kid;
            kid_to_toy[kid] = toy;
        }
        else if (play_time[kid][toy] < play_time[toy_to_kid[toy]][toy]){
            unmatched_kids.push_back(toy_to_kid[toy]);
            kid_to_toy[toy_to_kid[toy]] = -1;
            kid_to_toy[kid] = toy;
            toy_to_kid[toy] = kid;
        }else{
            unmatched_kids.push_back(kid);
        }
        
    }

    for (kid = 0; kid < kids; kid++){
        cout << kid_to_toy[kid]+1 << ' ';
    }
    cout << '\n';
}
