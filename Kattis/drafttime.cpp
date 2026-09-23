#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <queue>

using namespace std;

struct Player {
    string name;
    vector<string> prefs;
    int c;

    bool eq(const Player &other) const {
        return name == other.name;
    }

    bool operator<(const Player &other) const {
        return name < other.name;
    }
};

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);

    int n, m, k;
    cin >> n >> m >> k;

    vector<Player> undrafted(k);

    string team, player;
    map<string,pair<map<string,int>,priority_queue<pair<int,Player>>>> teams;
    for (int i {0}; i < n; i++) {
        cin >> team;
        map<string,int> prefs;
        for (int p {0}; p < k; p++) {
            cin >> player;
            prefs[player] = p;
        }
        teams[team] = {prefs,{}};
    }

    for (int i {0}; i < k; i++) {
        cin >> player;
        vector<string> prefs(n);
        for (int p {0}; p < n; cin >> prefs[p++]);
        undrafted[i] = Player{player,prefs,0};
    }

    while (undrafted.size()) {
        Player player = undrafted.back();
        undrafted.pop_back();

        if (player.c >= n) continue;

        string team = player.prefs[player.c++];
        int pri = teams[team].first[player.name];
        teams[team].second.push({pri,player});
        if (teams[team].second.size() > m) {
            auto [_, ejected] = teams[team].second.top();
            teams[team].second.pop();
            undrafted.push_back(ejected);
        }
    }

    for (auto [name, pq] : teams) {
        auto [_, team_queue] = pq;
        cout << name;
        while (team_queue.size()) {
            auto [_, player] = team_queue.top();
            team_queue.pop();
            cout << ' ' << player.name;
        }
        cout << '\n';
    }
}
