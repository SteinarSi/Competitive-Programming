#include <iostream>
#include <iomanip>
#include <queue>

using namespace std;

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);

    int c, s;
    cin >> c;
    priority_queue<int> queue;
    for (; c --> 0 ;) {
        cin >> s;
        queue.push(s);
    }
    int ret {0};
    vector<int> xs(5);
    while (queue.size() >= 5) {
        for (int i {0}; i < 5; xs[i++] = queue.top(), queue.pop());
        ret += 5;
        for (int i {0}; i < 5; i++) {
            if (xs[i] > 1) queue.push(xs[i]-1);
        }
    }
    ret += queue.size();
    cout << ret << '\n';
}
