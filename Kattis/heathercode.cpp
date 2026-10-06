#include <iostream>
#include <vector>

using namespace std;

int turn(int digit) {
    if (digit == 9) return 0;
    return digit + 1;
}

int combine(int x1, int x2, int x3, int x4, int x5, int x6, int x7) {
    return x1 + 10*x2 + 100*x3 + 1000*x4 + 10000*x5 + 100000*x6 + 1000000*x7;
}

int main() {
    int n;
    cin >> n;

    vector<bool> seen(100000000);

    int x1 = 0;
    int x2 = 0;
    int x3 = 0;
    int x4 = 0;
    int x5 = 0;
    int x6 = 0;
    int x7 = 0;
    for (; n --> 0 ;) {
        seen[combine(x1, x2, x3, x4, x5, x6, x7)] = true;
        if ( ! seen[combine(turn(x1), x2, x3, x4, x5, x6, x7)]) {
            x1 = turn(x1);
        }
        else if ( ! seen[combine(x1, turn(x2), x3, x4, x5, x6, x7)]) {
            x2 = turn(x2);
        }
        else if ( ! seen[combine(x1, x2, turn(x3), x4, x5, x6, x7)]) {
            x3 = turn(x3);
        }
        else if ( ! seen[combine(x1, x2, x3, turn(x4), x5, x6, x7)]) {
            x4 = turn(x4);
        }
        else if ( ! seen[combine(x1, x2, x3, x4, turn(x5), x6, x7)]) {
            x5 = turn(x5);
        }
        else if ( ! seen[combine(x1, x2, x3, x4, x5, turn(x6), x7)]) {
            x6 = turn(x6);
        }
        else if ( ! seen[combine(x1, x2, x3, x4, x5, x6, turn(x7))]) {
            x7 = turn(x7);
        }
        else break;
    }

    printf("%.7d\n", combine(x1, x2, x3, x4, x5, x6, x7));
}
