#include <iostream>
#include <string>
#include <vector>
#include <iomanip>
#include <fstream>
#include <algorithm>
#include <queue>

using namespace std;
using i64 = int64_t;

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);
    cout << setprecision(10) << fixed;

    std::priority_queue <i64> calories;

    ifstream in ("inputs/day1-input.txt");
    string line;
    i64 sum {0};
    while (! in.eof()){
        sum = 0;
        while (1){
            getline(in, line);
            if (line.length() == 0){
                break;
            }
            sum += stoi(line);
        }
        calories.push(sum);
    }
    int first = calories.top();
    calories.pop();
    int second = calories.top();
    calories.pop();
    cout << first << '\n';
    cout << first + second + calories.top() << '\n';
}
