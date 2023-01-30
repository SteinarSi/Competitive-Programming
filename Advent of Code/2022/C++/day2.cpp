#include <iostream>
#include <string>
#include <iomanip>
#include <fstream>

using namespace std;

int predetermined[3][3] = {
    {4, 8, 3},
    {1, 5, 9},
    {7, 2, 6}
};

int rigged[3][3] = {
    {3, 4, 8},
    {1, 5, 9},
    {2, 6, 7}
};

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);
    cout << setprecision(10) << fixed;

    ifstream in ("inputs/day2-input.txt");
    string line;
    int predetermined_sum {0};
    int rigged_sum {0};
    while (! in.eof()){
        getline(in, line);
        if (line.length() == 0) break;
        predetermined_sum += predetermined[int(line[0])-65][int(line[2])-88];
        rigged_sum += rigged[int(line[0])-65][int(line[2])-88];
    }
    cout << predetermined_sum << '\n' << rigged_sum << '\n';
}

