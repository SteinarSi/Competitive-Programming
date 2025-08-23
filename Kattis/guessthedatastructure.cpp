#include <iostream>
#include <iomanip>
#include <queue>
#include <stack>

using namespace std;

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    
    int n, t, x;
    while (cin >> n) {
        int is_fifo = true;
        int is_lifo = true;
        int is_pq   = true;
        int size = 0;
        queue<int> fifo;
        stack<int> lifo;
        priority_queue<int> pq;

        for (;n --> 0;) {
            cin >> t >> x;
            if (t == 1) {
                fifo.push(x);
                lifo.push(x);
                pq.push(x);
                size++;
            }
            else {
                if (size <= 0) {
                    is_fifo = 0;
                    is_lifo = 0;
                    is_pq = 0;
                }
                else {
                    if (fifo.front() != x) is_fifo = 0;
                    fifo.pop();
                    if (lifo.top() != x) is_lifo = 0;
                    lifo.pop();
                    if (pq.top() != x) is_pq = 0;
                    pq.pop();
                    size--;
                }
            }
        }

        if (is_fifo + is_lifo + is_pq >= 2) cout << "not sure\n";
        else if (is_fifo) cout << "queue\n";
        else if (is_lifo) cout << "stack\n";
        else if (is_pq) cout << "priority queue\n";
        else cout << "impossible\n";
    }
}
