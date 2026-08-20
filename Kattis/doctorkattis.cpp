#include <iostream>
#include <iomanip>
#include <set>
#include <map>

using namespace std;

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);

    int n, q, x;
    string name;
    map<string,pair<int,int>> cats;
    set<pair<pair<int,int>,string>> queue;
    int ids {0};
    cin >> n;

    for (; n --> 0 ;) {
        cin >> q;
        if (q == 0) {
            cin >> name >> x;
            cats[name] = {x, ids};
            queue.insert({{x,ids--},name});
        }
        else if (q == 1) {
            cin >> name >> x;
            auto [inf,id] = cats[name];
            queue.erase({{inf,id},name});
            queue.insert({{inf+x,id},name});
            cats[name] = {inf+x,id};
        }
        else if (q == 2) {
            cin >> name;
            auto [inf,id] = cats[name];
            queue.erase({{inf,id},name});
            cats.erase(name);
        }
        else if (q == 3) {
            if (queue.size()) cout << queue.rbegin()->second << '\n';
            else cout << "The clinic is empty\n";
        }
    }
}
