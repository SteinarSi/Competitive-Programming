#include <iostream>
#include <vector>
#include <algorithm>
#include <string>
#include <map>

using namespace std;

int read_input(int n, vector<vector<pair<int,double>>> &graph){
    map<string,int> name_to_id;
    vector<string> id_to_name;
    auto get_id = [&](const string &s) {
        auto [it, placed] = name_to_id.emplace(s, name_to_id.size());
        if (placed) id_to_name.push_back(s);
        return it->second;
    };

    int ceo, id1, id2;
    vector<double> speeds(n);
    double speed;
    string a, b;
    vector<pair<int, double>> temp_graph;
    for (int i {0}; i < n; i++){
        cin >> a >> speed >> b;
        id1 = get_id(a);
        speeds[id1] = speed;
        if (b != "CEO"){
            id2 = get_id(b);
            temp_graph.push_back({id1, id2});
        }else ceo = id1;
    }

    for (auto [emp, sup] : temp_graph){
        graph[sup].push_back({emp, min(speeds[emp], speeds[sup])});
    }
    return ceo;
}

// Compute DP table of largest matchings including and not including u.
void maximum_cardinal_matching(int u, vector<vector<pair<int,double>>> &graph, vector<int> &count_with, vector<int> &count_without){
    if (graph[u].size() == 0){
        count_with[u] = 0;
        count_without[u] = 0;
        return;
    }   
    int count_all = 0;
    int best_emp = -999999;
    for (auto [emp, w] : graph[u]){
        maximum_cardinal_matching(emp, graph, count_with, count_without);
        count_all += max(count_with[emp], count_without[emp]);
        best_emp = max(best_emp, 1 + count_without[emp] - max(count_with[emp], count_without[emp]));
    }
    count_without[u] = count_all;
    count_with[u] = count_all + best_emp;
}

// Compute DP table of maximum weighted matching with or without u.
void maximum_weighted_cardinal_matching(int u, vector<vector<pair<int,double>>> &graph, vector<int> &count_with, vector<int> &count_without, vector<double> &sum_with, vector<double> &sum_without){
    if (graph[u].size() == 0){
        sum_with[u] = 0.0;
        sum_without[u] = 0.0;
        return;
    }
    double sum_with_all = 0;
    double best_sum_with = -99999;
    for (auto [emp, w] : graph[u]){
        maximum_weighted_cardinal_matching(emp, graph, count_with, count_without, sum_with, sum_without);
        if      (count_with[emp] > count_without[emp]) sum_with_all += sum_with[emp];
        else if (count_with[emp] < count_without[emp]) sum_with_all += sum_without[emp];
        else                                           sum_with_all += max(sum_with[emp], sum_without[emp]);

        // Can I pick this edge and still get a maximal cardinal matching?
        if (count_without[emp] + 1 - max(count_with[emp], count_without[emp]) >= 0){
            best_sum_with = max(best_sum_with, w - max(sum_with[emp], sum_without[emp]) + sum_without[emp]);
        }
    }

    sum_without[u] = sum_with_all;
    sum_with[u] = sum_with_all + best_sum_with;
}

void solve(int id, vector<vector<pair<int,double>>> &graph, vector<int> &count_with, vector<int> &count_without, vector<double> &sum_with, vector<double> &sum_without){
    if (graph[id].size() == 0){
        count_with[id] = 0;
        count_without[id] = 0;
        sum_with[id] = 0.0;
        sum_without[id] = 0.0;
        return;
    }
    for (auto [emp, w] : graph[id]){
        solve(emp, graph, count_with, count_without, sum_with, sum_without);
    }

    int count_with_all = 0;
    double sum_with_all = 0.0;
    int best_without = -1;
    int smallest_diff = 9999;
    for (auto [emp, w] : graph[id]){
        count_with_all += max(count_with[emp], count_without[emp]);
        sum_with_all += max(sum_with[emp], sum_without[emp]);
        smallest_diff = min(smallest_diff, count_with[emp] - count_without[emp]);
    }
    count_without[id] = count_with_all;
    count_with[id] = 1 + count_with_all - smallest_diff;

    sum_without[id] = 0.0;
    for (auto [emp, w] : graph[id]){
        if      (count_with[emp] > count_without[emp]) sum_without[id] += sum_with[emp];
        else if (count_with[emp] < count_without[emp]) sum_without[id] += sum_without[emp];
        else                                           sum_without[id] += max(sum_with[emp], sum_without[emp]);
    }
    for (auto [emp, w] : graph[id]){
        if (count_without[emp] + 1 - max(count_with[emp], count_without[emp]) >= 0){
            sum_with[id] = max(sum_with[id], sum_with_all - max(sum_with[emp], sum_without[emp]) + sum_without[emp] + w);
        }
    }
}


int main(){
    int n;
    cin >> n;
    vector<vector<pair<int,double>>> graph(n);
    int ceo = read_input(n, graph);
    
    vector<int> count_with(n);
    vector<int> count_without(n);
    maximum_cardinal_matching(ceo, graph, count_with, count_without);


    vector<double> sum_with(n);
    vector<double> sum_without(n);
    maximum_weighted_cardinal_matching(ceo, graph, count_with, count_without, sum_with, sum_without);

    int s;
    double a;
    if      (count_with[ceo] > count_without[ceo]) { s = count_with[ceo]; a = sum_with[ceo]; }
    else if (count_with[ceo] < count_without[ceo]) { s = count_without[ceo]; a = sum_without[ceo]; }
    else { s = count_with[ceo]; a = max(sum_with[ceo], sum_without[ceo]); }
    cout << s << ' ' << a / (double) s << '\n';
}