#include <iostream>
#include <string>
#include <vector>
#include <iomanip>

using namespace std;

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);

    string x, a, b;
    vector<string> stack;
    while (cin >> x) {
        if (x[0] >= '0') stack.push_back(x);
        else {
            stack[stack.size()-2] = "(" + stack[stack.size()-2] + x + stack[stack.size()-1] + ")";
            stack.pop_back();
        }
    }
    cout << stack[0] << '\n';
}
